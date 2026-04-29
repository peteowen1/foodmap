#' Read cached HTML response for a URL
#' @param url Character. The URL that was fetched.
#' @param max_age_hours Numeric. Maximum cache age in hours. Default 24.
#' @param cache_dir Character. Cache directory path. Default "cache".
#' @return Cached HTML string, or NULL if not cached or expired.
#' @noRd
cache_get <- function(url, max_age_hours = 24, cache_dir = "cache") {
  path <- cache_path(url, cache_dir)
  if (!file.exists(path)) return(NULL)

  age_hours <- difftime(Sys.time(), file.mtime(path), units = "hours")
  if (as.numeric(age_hours) > max_age_hours) return(NULL)

  readLines(path, warn = FALSE) |> paste(collapse = "\n")
}

#' Save HTML response to cache
#' @param url Character. The URL that was fetched.
#' @param html Character. The HTML response body.
#' @param cache_dir Character. Cache directory path. Default "cache".
#' @noRd
cache_set <- function(url, html, cache_dir = "cache") {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  writeLines(html, cache_path(url, cache_dir))
  invisible(NULL)
}

#' Build cache file path from URL
#' @noRd
cache_path <- function(url, cache_dir = "cache") {
  hash <- substr(rlang::hash(url), 1, 16)
  file.path(cache_dir, paste0(hash, ".html"))
}

#' Fetch a URL with optional caching
#'
#' Wraps the common httr2 fetch pattern. When caching is enabled, returns
#' cached HTML if fresh, otherwise fetches and caches the response.
#'
#' @param url Character. URL to fetch.
#' @param use_cache Logical. Whether to use the cache.
#' @param max_age_hours Numeric. Maximum cache age in hours. Default 24.
#' @param extra_headers Named list of additional HTTP headers (e.g.
#'   `Referer`, `Accept-Language`) layered on top of the default
#'   User-Agent. Sites behind aggressive CDNs (Michelin Guide) need
#'   these to avoid soft-blocked responses.
#' @param validate Optional function `(html) -> logical`. Runs on both
#'   fresh fetches and cache hits. If it returns anything other than
#'   `TRUE`, the response is treated as invalid: cache hits are
#'   discarded and a fresh fetch is attempted, fresh fetches throw
#'   an error and are *not* written to cache. Use this to reject bot
#'   challenges (e.g. AWS WAF) that return HTTP 200 with placeholder
#'   bodies — without validation those poison the cache permanently.
#' @return HTML response body as a character string.
#' @noRd
cached_fetch <- function(url, use_cache = FALSE, max_age_hours = 24,
                         extra_headers = NULL, validate = NULL) {
  is_valid <- function(html) {
    if (is.null(validate)) return(TRUE)
    isTRUE(tryCatch(validate(html), error = function(e) FALSE))
  }

  if (use_cache) {
    cached <- cache_get(url, max_age_hours = max_age_hours)
    if (!is.null(cached) && is_valid(cached)) {
      cli::cli_alert_info("Using cached response for {.url {url}}")
      return(cached)
    }
  }

  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = user_agent_string()) |>
    httr2::req_retry(max_tries = 3)
  if (length(extra_headers) > 0) {
    req <- do.call(httr2::req_headers, c(list(req), as.list(extra_headers)))
  }
  html <- req |> httr2::req_perform() |> httr2::resp_body_string()

  if (!is_valid(html)) {
    cli::cli_abort(
      "Response from {.url {url}} failed validation (likely a bot challenge)"
    )
  }

  if (use_cache) {
    cache_set(url, html)
  }

  html
}
