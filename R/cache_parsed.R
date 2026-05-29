# Parsed-tibble cache for scraper output -------------------------------------
#
# Layered ABOVE the HTML cache. Saves the parsed tibble result of each
# scrape_*(city) call to cache/parsed/{key}.rds plus a sidecar manifest
# listing the HTML cache files the parse used. On the next run, if every
# manifest entry's mtime still matches the on-disk HTML cache file, we
# skip parsing entirely and return the stored tibble.
#
# Storage format: RDS. Justified per CLAUDE.md's "no RDS" carve-out for
# R-specific object types - tibbles carry class metadata and source-typed
# columns that don't round-trip cleanly through CSV/parquet without a
# schema declaration. The cache is internal and never shipped externally.
#
# Tracking strategy: cached_fetch() in R/cache.R records each URL it
# serves (fresh or cache hit) into a package-level tracker env when the
# tracker is active. cached_scrape() activates the tracker around the
# expr eval, collects the recorded URLs at the end, and writes them to
# the manifest. Scrapers that bypass cached_fetch() (chromote, in-memory
# lists) leave the URL list empty - those fall back to a TTL on the
# parsed cache file itself.


# Package-level URL tracker. Single mutable bucket; only one cached_scrape
# can be active at a time (we don't currently spawn parallel scrapers).
.foodmap_url_tracker <- new.env(parent = emptyenv())
.foodmap_url_tracker$urls <- character()
.foodmap_url_tracker$active <- FALSE


#' Begin recording URLs fetched via cached_fetch()
#' @noRd
cache_track_start <- function() {
  .foodmap_url_tracker$urls <- character()
  .foodmap_url_tracker$active <- TRUE
}


#' Stop recording and return the unique URL list
#' @noRd
cache_track_stop <- function() {
  .foodmap_url_tracker$active <- FALSE
  urls <- .foodmap_url_tracker$urls
  .foodmap_url_tracker$urls <- character()
  urls
}


#' Record a URL (called from cached_fetch on every retrieval)
#' @noRd
cache_track_record <- function(url) {
  if (isTRUE(.foodmap_url_tracker$active)) {
    .foodmap_url_tracker$urls <- unique(
      c(.foodmap_url_tracker$urls, url)
    )
  }
}


#' Wrap a scraper call with parse-result caching
#'
#' @param key Cache key, conventionally `"{source}_{city}"` (e.g.
#'   `"broadsheet_sydney"`).
#' @param expr Unevaluated expression that produces the parsed tibble.
#'   Wrapped in `force()` here so lazy evaluation defers until after
#'   the cache check.
#' @param cache_dir Directory for parsed-cache files. Defaults to
#'   `"cache/parsed"`.
#' @param html_cache_dir Directory the HTML cache lives in. Manifest
#'   path computations use this so the invalidation check looks at
#'   the same files cached_fetch() wrote. Defaults to `"cache"`, which
#'   matches the production cached_fetch() default. Override in tests
#'   so manifest entries point at the temp HTML cache, not the real
#'   one.
#' @param ttl_hours Fallback TTL used when the URL manifest is empty
#'   (chromote scrapers, in-memory data sources). Defaults to 24h,
#'   mirroring the HTML cache TTL.
#'
#' @return The parsed tibble (either freshly evaluated or restored).
#' @noRd
cached_scrape <- function(key, expr, cache_dir = "cache/parsed",
                          html_cache_dir = "cache",
                          ttl_hours = 24) {
  rds_path <- file.path(cache_dir, paste0(key, ".rds"))
  manifest_path <- file.path(cache_dir, paste0(key, ".manifest.json"))

  if (parsed_cache_valid(rds_path, manifest_path, ttl_hours)) {
    cli::cli_alert_info("Using parsed cache for {.val {key}}")
    return(readRDS(rds_path))
  }

  cache_track_start()
  # If expr aborts (scraper failure) we still need to deactivate the
  # tracker so the next scraper run starts clean. force() guarantees
  # we evaluate exactly once.
  on.exit(cache_track_stop(), add = TRUE)

  result <- force(expr)

  urls_touched <- cache_track_stop()
  # Cancel the on.exit since we've already stopped cleanly. Calling
  # cache_track_stop() twice would also be fine (idempotent), but
  # being explicit makes the control flow easier to reason about.
  on.exit()

  manifest <- build_parsed_manifest(urls_touched, html_cache_dir)
  manifest$code_mtime <- newest_code_mtime()

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(result, rds_path)
  writeLines(
    jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE),
    manifest_path
  )

  result
}


#' Construct a manifest of URL + cache-file + mtime triples
#'
#' jsonlite round-trips a data.frame as an array of objects, which is
#' what we want here. Empty URL list still produces a valid manifest
#' (n_urls = 0) so cache misses are distinguishable from track failures.
#' @noRd
build_parsed_manifest <- function(urls, html_cache_dir = "cache") {
  if (length(urls) == 0) {
    return(list(
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      n_urls = 0L,
      urls = list()
    ))
  }
  cache_paths <- vapply(urls, function(u) cache_path(u, html_cache_dir),
                        character(1))
  # Defensive: only manifest entries whose cache file actually exists.
  # cached_fetch() already filters at the track site, but a scraper that
  # calls cache_track_record() directly could still hand us an absent
  # URL. A NA-mtime entry would force validation to fail forever on the
  # next run.
  present <- file.exists(cache_paths)
  urls <- urls[present]
  cache_paths <- cache_paths[present]
  if (length(urls) == 0) {
    return(list(
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      n_urls = 0L,
      urls = list()
    ))
  }
  mtimes <- vapply(cache_paths,
                   function(cp) format(file.mtime(cp), "%Y-%m-%dT%H:%M:%S"),
                   character(1))
  list(
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    n_urls = length(urls),
    urls = data.frame(
      url = urls,
      cache_path = cache_paths,
      mtime = mtimes,
      stringsAsFactors = FALSE
    )
  )
}


#' Validate a parsed-cache file against its manifest
#'
#' Returns TRUE when the cache is safe to reuse, FALSE on any signal
#' that the source HTML may have moved:
#'   * Either file missing
#'   * Manifest malformed
#'   * Any tracked URL's HTML cache file deleted
#'   * Any tracked URL's HTML cache mtime changed since the parse ran
#'   * (Empty URL list only) parsed cache older than ttl_hours
#' @noRd
parsed_cache_valid <- function(rds_path, manifest_path, ttl_hours) {
  if (!file.exists(rds_path)) return(FALSE)
  if (!file.exists(manifest_path)) return(FALSE)

  manifest <- tryCatch(
    jsonlite::fromJSON(manifest_path, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(manifest)) return(FALSE)

  # Invalidate when any R/*.R file has been touched since the cache
  # was written. Catches scraper-logic changes that mtime-based URL
  # invalidation can't see - e.g. adding new sections to a multi-pull
  # scraper while the previously-tracked URLs all stay unchanged.
  # Older caches without a code_mtime field skip this check (treat as
  # valid pre-feature behaviour).
  if (!is.null(manifest$code_mtime)) {
    cur_code <- newest_code_mtime()
    if (!is.na(cur_code) && !is.na(manifest$code_mtime) &&
        cur_code > manifest$code_mtime) {
      return(FALSE)
    }
  }

  n_urls <- manifest$n_urls %||% 0L
  if (n_urls > 0 && is.data.frame(manifest$urls)) {
    cps <- manifest$urls$cache_path
    expected_mtimes <- manifest$urls$mtime
    for (i in seq_along(cps)) {
      cp <- cps[i]
      if (!file.exists(cp)) return(FALSE)
      cur <- format(file.mtime(cp), "%Y-%m-%dT%H:%M:%S")
      if (!identical(cur, expected_mtimes[i])) return(FALSE)
    }
    return(TRUE)
  }

  # No tracked URLs - fall back to TTL on the parsed cache file itself.
  rds_age_hours <- as.numeric(
    difftime(Sys.time(), file.mtime(rds_path), units = "hours")
  )
  rds_age_hours <= ttl_hours
}


#' Newest mtime across all R/*.R files as an ISO-8601 string
#'
#' Used as a coarse "package version" fingerprint in parsed-cache
#' manifests so any source-code change (added section, fixed parse
#' bug, schema tweak) invalidates the parsed caches even when the
#' tracked HTML URLs are unchanged. Returns NA when no R files are
#' found (running outside a package checkout) - the validator treats
#' that as "skip the check".
#' @noRd
newest_code_mtime <- function(r_dir = "R") {
  if (!dir.exists(r_dir)) return(NA_character_)
  files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  format(max(file.mtime(files)), "%Y-%m-%dT%H:%M:%S")
}
