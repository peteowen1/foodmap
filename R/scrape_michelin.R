#' Scrape the Michelin Guide
#'
#' Pulls the full restaurant set for a city — every tier from Selected
#' through Bib Gourmand and One/Two/Three Stars. The listing pages are
#' server-rendered HTML with a flat link-card layout, and detail pages
#' embed a clean `Restaurant` JSON-LD block (address, latitude/longitude,
#' cuisine, star rating, award year, telephone, description). No
#' headless browser required.
#'
#' Michelin sits behind CloudFront and silently bounces requests with a
#' bare User-Agent (HTTP 202 with empty body), so the scraper layers in
#' `Accept`, `Accept-Language`, and `Referer` headers. The Guide
#' refreshes annually, so caching defaults to 30 days rather than the
#' usual 24 hours — re-running with `use_cache = TRUE` finishes in
#' seconds and never re-fetches.
#'
#' @param city Character. Lowercase city slug. Currently supported:
#'   `"san-francisco"`. Default `"san-francisco"`.
#' @param use_cache Logical. Cache HTTP responses on disk. Default
#'   `FALSE`.
#' @param max_age_hours Numeric. Cache freshness window in hours when
#'   `use_cache = TRUE`. Default `24 * 30` (30 days).
#'
#' @return A tibble with the standard scraper schema plus
#'   `michelin_distinction` (one of "Three Stars", "Two Stars",
#'   "One Star", "Bib Gourmand", "Selected"), `michelin_year`,
#'   `phone`, and `cost_range` columns.
#' @export
scrape_michelin <- function(city = "san-francisco",
                            use_cache = FALSE,
                            max_age_hours = 24 * 30) {
  city <- validate_city_source(city, "michelin")
  cli::cli_h1("Scraping Michelin Guide: {city}")

  base_url <- michelin_listing_url(city)
  url_prefix <- michelin_url_prefix(city)
  cli::cli_alert_info("Discovering venue URLs from listing pages...")
  detail_urls <- michelin_collect_detail_urls(
    base_url,
    url_prefix = url_prefix,
    use_cache = use_cache,
    max_age_hours = max_age_hours
  )

  if (length(detail_urls) == 0) {
    cli::cli_abort("No restaurant URLs found on Michelin listing pages.")
  }
  cli::cli_alert_info(
    "Found {length(detail_urls)} venue{?s}; fetching detail pages..."
  )

  # Michelin's CDN runs an AWS WAF that flips into bot-challenge mode
  # at roughly 2-3 req/sec. Sustained 1 req/sec stays under the
  # threshold reliably. Failed pages get one slow retry at the end.
  n <- length(detail_urls)
  raw <- purrr::imap(detail_urls, function(url, idx) {
    if (idx %% 25 == 0) cli::cli_alert_info("  ...{idx}/{n}")
    Sys.sleep(MICHELIN_RATE_SECS)
    tryCatch(
      michelin_parse_detail(
        url,
        use_cache = use_cache,
        max_age_hours = max_age_hours
      ),
      error = function(e) NULL
    )
  })

  failed_idx <- which(purrr::map_lgl(raw, is.null))
  if (length(failed_idx) > 0) {
    cli::cli_alert_info(
      "Retrying {length(failed_idx)} page{?s} that failed (likely WAF challenges)..."
    )
    for (i in failed_idx) {
      Sys.sleep(MICHELIN_RATE_SECS * 3)
      raw[[i]] <- tryCatch(
        michelin_parse_detail(
          detail_urls[[i]],
          use_cache = use_cache,
          max_age_hours = max_age_hours
        ),
        error = function(e) NULL
      )
    }
  }
  n_failed <- sum(purrr::map_lgl(raw, is.null))
  if (n_failed > 0) {
    cli::cli_warn("{n_failed}/{n} detail page{?s} failed to fetch or parse")
  }
  rows <- purrr::compact(raw)
  if (length(rows) == 0) {
    cli::cli_abort("Could not extract any venues from the Michelin Guide.")
  }

  result <- dplyr::bind_rows(rows)
  starred <- sum(result$michelin_distinction != "Selected", na.rm = TRUE)
  cli::cli_alert_success(
    "Found {nrow(result)} Michelin venue{?s} ({starred} with stars or Bib)"
  )
  result
}


#' Per-request pause for Michelin Guide scraping
#'
#' Michelin's AWS WAF will start serving bot-challenge pages (HTTP 200
#' bodies of ~2 KB containing `AwsWafIntegration`) once it sees more
#' than ~2 requests per second sustained. 1 second between requests
#' stays comfortably under the threshold.
#' @noRd
MICHELIN_RATE_SECS <- 1.0


#' Validator for a Michelin detail-page response
#'
#' Returns `TRUE` only if the body looks like a real venue page —
#' contains a `Restaurant` JSON-LD block and is not the AWS WAF
#' challenge HTML. Used by `cached_fetch()` to refuse poisoned
#' cache hits and decline to cache fresh WAF responses.
#' @noRd
michelin_response_ok <- function(html) {
  !grepl("AwsWafIntegration", html, fixed = TRUE) &&
    grepl('"@type":"Restaurant"', html, fixed = TRUE)
}


#' City -> Michelin Guide listing URL
#' @noRd
michelin_listing_url <- function(city) {
  switch(city,
    `san-francisco` = "https://guide.michelin.com/us/en/california/san-francisco/restaurants",
    cli::cli_abort("No Michelin Guide URL configured for {.val {city}}")
  )
}


#' URL-path prefix that limits a listing scrape to one region
#'
#' The Michelin Guide listing page mixes the city's own venue cards with
#' "Featured" / sidebar promo links pointing to venues in other cities
#' and states (e.g. New York from the SF listing). Without filtering,
#' those leak into our discovered URL set and get scraped as if they
#' belonged to the city. Each city resolves to the path prefix we keep
#' — for SF that's the whole California region, since the SF Michelin
#' Guide legitimately covers Bay Area satellites (Berkeley, Oakland,
#' Healdsburg, etc.).
#' @noRd
michelin_url_prefix <- function(city) {
  switch(city,
    `san-francisco` = "/us/en/california/",
    cli::cli_abort("No Michelin URL prefix configured for {.val {city}}")
  )
}


#' Headers that satisfy Michelin's CloudFront bot defenses
#'
#' The site returns HTTP 202 with an empty body for requests that
#' lack a realistic Accept / Accept-Language pair, and pagination
#' beyond page 1 additionally requires a Referer.
#' @noRd
michelin_headers <- function(referer = NULL) {
  base <- list(
    Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    `Accept-Language` = "en-US,en;q=0.9"
  )
  if (!is.null(referer)) base$Referer <- referer
  base
}


#' Walk paginated listing pages and collect detail URLs
#'
#' Pages are at `/restaurants` (page 1) and `/restaurants/page/N` for
#' N >= 2. The site returns 200 with an empty card list once it runs
#' out of pages, which we detect by zero card extractions. A hard cap
#' guards against an infinite loop if the markup ever changes shape.
#' @noRd
michelin_collect_detail_urls <- function(base_url, url_prefix,
                                         use_cache, max_age_hours) {
  # Listing pages don't have a Restaurant JSON-LD block, so the
  # detail-page validator would over-reject them. Use a simpler
  # listing-page validator that just rejects WAF challenges.
  listing_ok <- function(html) {
    !grepl("AwsWafIntegration", html, fixed = TRUE) &&
      grepl("card__menu", html, fixed = TRUE)
  }

  detail_urls <- character()
  page <- 1L
  prev_url <- NULL
  repeat {
    page_url <- if (page == 1L) base_url else paste0(base_url, "/page/", page)
    html_str <- tryCatch(
      cached_fetch(
        page_url,
        use_cache = use_cache,
        max_age_hours = max_age_hours,
        extra_headers = michelin_headers(referer = prev_url %||% base_url),
        validate = listing_ok
      ),
      error = function(e) ""
    )
    new_urls <- michelin_extract_card_urls(html_str, url_prefix = url_prefix)
    if (length(new_urls) == 0) break
    detail_urls <- unique(c(detail_urls, new_urls))
    prev_url <- page_url
    page <- page + 1L
    if (page > 25L) break
    Sys.sleep(MICHELIN_RATE_SECS)
  }
  detail_urls
}


#' Extract restaurant detail URLs from a listing page's HTML
#'
#' Filters to URLs whose path begins with `url_prefix` so cross-promo
#' links to venues in other regions (e.g. New York featured cards on
#' the SF listing) get dropped.
#' @noRd
michelin_extract_card_urls <- function(html_str, url_prefix) {
  if (!nzchar(html_str)) return(character())
  m <- stringr::str_match_all(
    html_str,
    'href="(/[a-z]{2}/[a-z]{2}/[^"]+/restaurant/[^"]+)"'
  )[[1]]
  if (nrow(m) == 0) return(character())
  paths <- unique(m[, 2])
  paths <- paths[startsWith(paths, url_prefix)]
  if (length(paths) == 0) return(character())
  paste0("https://guide.michelin.com", paths)
}


#' Parse a single Michelin detail page into one tibble row
#' @noRd
michelin_parse_detail <- function(url, use_cache, max_age_hours) {
  html_str <- cached_fetch(
    url,
    use_cache = use_cache,
    max_age_hours = max_age_hours,
    extra_headers = michelin_headers(referer = "https://guide.michelin.com/"),
    validate = michelin_response_ok
  )
  page <- rvest::read_html(html_str)
  ld_text <- rvest::html_elements(page, "script[type='application/ld+json']") |>
    rvest::html_text()

  ld <- NULL
  for (jt in ld_text) {
    parsed <- tryCatch(
      jsonlite::fromJSON(jt, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (!is.null(parsed) && identical(parsed$`@type`, "Restaurant")) {
      ld <- parsed
      break
    }
  }
  if (is.null(ld)) return(NULL)

  addr_obj <- ld$address %||% list()
  street   <- addr_obj$streetAddress %||% NA_character_
  locality <- addr_obj$addressLocality %||% NA_character_
  postcode <- addr_obj$postalCode %||% NA_character_
  region   <- addr_obj$addressRegion %||% NA_character_
  full_address <- paste(
    Filter(function(x) !is.na(x) && nzchar(x), c(street, locality, region, postcode)),
    collapse = ", "
  )
  if (!nzchar(full_address)) full_address <- NA_character_

  star_text   <- ld$starRating %||% ld$award$awardFor %||% NA_character_
  distinction <- michelin_normalize_distinction(star_text)

  award_year <- suppressWarnings(as.integer(ld$award$dateAwarded %||% NA))

  price_text <- ld$priceRange %||% NA_character_
  price_int  <- michelin_price_to_int(price_text)

  description <- ld$review$description %||% NA_character_

  tibble::tibble(
    name                 = ld$name %||% NA_character_,
    suburb               = locality,
    address              = full_address,
    cuisine              = ld$servesCuisine %||% NA_character_,
    category             = "Restaurant",
    description          = description,
    price_range          = price_int,
    cost_range           = price_text,
    rating               = NA_real_,
    rating_scale         = NA_character_,
    michelin_distinction = distinction,
    michelin_year        = award_year,
    phone                = ld$telephone %||% NA_character_,
    latitude             = suppressWarnings(as.numeric(ld$latitude %||% NA)),
    longitude            = suppressWarnings(as.numeric(ld$longitude %||% NA)),
    url                  = ld$url %||% url
  )
}


#' Map Michelin's verbose star strings to short distinction labels
#'
#' Detail-page JSON-LD spells the tier out ("One Star: High quality
#' cooking"); Selected venues have no `starRating` field at all, so
#' absence is treated as "Selected" since the venue still appeared on
#' the city listing.
#' @noRd
michelin_normalize_distinction <- function(star_text) {
  if (is.null(star_text) || is.na(star_text) || !nzchar(star_text)) {
    return("Selected")
  }
  s <- tolower(star_text)
  if (grepl("three star", s))    return("Three Stars")
  if (grepl("two star", s))      return("Two Stars")
  if (grepl("one star", s))      return("One Star")
  if (grepl("bib gourmand", s))  return("Bib Gourmand")
  "Selected"
}


#' Map Michelin's textual price tier to a 1-4 integer
#'
#' Observed price strings on detail pages:
#'   "Easy on the wallet"  -> 1   (budget)
#'   "A reasonable spend"  -> 2
#'   "Special occasion"    -> 3
#'   "Spare no expense"    -> 4   (premium)
#' Anything unrecognised stays NA so callers can fall back to the
#' raw `cost_range` string instead.
#' @noRd
michelin_price_to_int <- function(price_text) {
  if (is.null(price_text) || is.na(price_text)) return(NA_integer_)
  s <- tolower(price_text)
  if (grepl("spare no expense", s))                   return(4L)
  if (grepl("special occasion", s))                   return(3L)
  if (grepl("reasonable", s))                         return(2L)
  if (grepl("easy on the wallet|wallet|pocket", s))   return(1L)
  NA_integer_
}
