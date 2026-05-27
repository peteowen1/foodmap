#' Scrape The World's 50 Best Restaurants
#'
#' W50B publishes a server-rendered ranked list (1-50 + 51-100) of
#' the world's top restaurants. Each list page is just a grid of
#' `<div class="list-item">` cards carrying rank, venue name, city
#' and a link to a detail page; each detail page embeds clean
#' `Restaurant` JSON-LD with the street address, founder/chef, award
#' citations and a short description.
#'
#' City filtering happens by name match against the detail page's
#' `addressLocality` - so an `"new-york"` scrape keeps only the NYC
#' entries, drops Tokyo, Lima, Barcelona, etc. Geocoding fills in
#' coordinates afterward (no lat/lng in the JSON-LD).
#'
#' Detail pages are cheap to cache (the awards change once a year)
#' so `max_age_hours` defaults to 30 days when caching is enabled.
#'
#' @param city Character. Lowercase city slug used by the geocoder.
#'   The scraper resolves it to one or more "locality" strings that
#'   W50B uses in the JSON-LD address field. Default `"new-york"`.
#' @param use_cache Logical. Cache pages on disk. Default `FALSE`.
#' @param max_age_hours Numeric. Cache freshness window when
#'   `use_cache = TRUE`. Default `24 * 30` (30 days - the list
#'   refreshes annually).
#'
#' @return A tibble with the standard scraper schema, plus an
#'   `award_year` integer column (the year the venue won/placed).
#' @export
scrape_worlds50best <- function(city = "new-york",
                                use_cache = FALSE,
                                max_age_hours = 24 * 30) {
  city <- validate_city_source(city, "worlds50best")
  cli::cli_h1("Scraping World's 50 Best: {city}")

  list_urls <- c(
    "https://www.theworlds50best.com/list/1-50",
    "https://www.theworlds50best.com/list/51-100"
  )
  detail_urls <- character()
  for (lu in list_urls) {
    cli::cli_alert_info("Discovering venues from {.url {lu}}")
    Sys.sleep(RATE_LIMIT_SECS)
    html_str <- tryCatch(
      cached_fetch(lu, use_cache = use_cache, max_age_hours = max_age_hours),
      error = function(e) {
        cli::cli_warn("List page {.url {lu}} failed: {conditionMessage(e)}")
        ""
      }
    )
    detail_urls <- c(detail_urls, w50b_extract_detail_urls(html_str))
  }
  detail_urls <- unique(detail_urls)
  if (length(detail_urls) == 0) {
    cli::cli_abort("No venue links extracted from W50B listing pages.")
  }
  cli::cli_alert_info(
    "Found {length(detail_urls)} venue link{?s}; fetching detail pages..."
  )

  city_localities <- tolower(w50b_city_localities(city))
  rows <- list()
  n <- length(detail_urls)
  for (i in seq_along(detail_urls)) {
    if (i %% 25 == 0) cli::cli_alert_info("  ...{i}/{n}")
    Sys.sleep(RATE_LIMIT_SECS)
    parsed <- tryCatch(
      w50b_parse_detail(detail_urls[i], use_cache = use_cache,
                        max_age_hours = max_age_hours),
      error = function(e) NULL
    )
    if (is.null(parsed)) next
    locality <- tolower(parsed$suburb %||% "")
    if (!nzchar(locality)) next
    if (!any(locality == city_localities)) next
    rows[[length(rows) + 1L]] <- parsed
  }

  if (length(rows) == 0) {
    cli::cli_warn("No W50B venues matched {.val {city}}")
    return(empty_restaurant_tibble() |>
             dplyr::mutate(award_year = integer()))
  }

  result <- dplyr::bind_rows(rows)
  cli::cli_alert_success("Found {nrow(result)} venue{?s} in {city}")
  result
}


#' City slug -> W50B `addressLocality` candidates
#'
#' W50B's JSON-LD `addressLocality` is the venue's city name. Most
#' cities map 1:1 to a single string (`"New York"`, `"London"`,
#' `"Tokyo"`), but a few cover sibling localities (LA includes
#' Beverly Hills, Santa Monica, etc.; SF includes Oakland).
#' @noRd
w50b_city_localities <- function(city) {
  switch(city,
    `new-york`      = c("New York"),
    `los-angeles`   = c("Los Angeles", "Beverly Hills", "Santa Monica",
                        "Culver City", "Pasadena", "Venice", "West Hollywood"),
    `san-francisco` = c("San Francisco", "Oakland", "Berkeley", "Healdsburg"),
    london          = c("London"),
    cli::cli_abort("No W50B localities configured for {.val {city}}")
  )
}


#' Extract `/the-list/<slug>.html` URLs from a W50B list page
#' @noRd
w50b_extract_detail_urls <- function(html_str) {
  if (!nzchar(html_str)) return(character())
  m <- stringr::str_match_all(
    html_str, 'href="(/the-list/[^"]+\\.html)"'
  )[[1]]
  if (nrow(m) == 0) return(character())
  paths <- unique(m[, 2])
  paste0("https://www.theworlds50best.com", paths)
}


#' Parse a W50B detail page into a single tibble row
#' @noRd
w50b_parse_detail <- function(url, use_cache, max_age_hours) {
  html_str <- cached_fetch(url, use_cache = use_cache,
                           max_age_hours = max_age_hours)

  ld <- w50b_extract_jsonld_restaurant(html_str)
  if (is.null(ld)) return(NULL)

  addr <- ld$address %||% list()
  street   <- addr$streetAddress %||% NA_character_
  locality <- addr$addressLocality %||% NA_character_

  # Awards live in the JSON-LD as an array of citation strings like
  # "The World's 50 Best Restaurants 2025, No. 1" - pull the year out
  # of whichever citation has one (most do).
  award_year <- w50b_extract_award_year(ld$award)

  awards_blurb <- if (length(ld$award) > 0) {
    paste(unlist(ld$award), collapse = "; ")
  } else {
    NA_character_
  }
  description <- ld$description %||% NA_character_
  full_blurb <- if (!is.na(description) && nzchar(description)) {
    if (!is.na(awards_blurb)) paste(description, awards_blurb, sep = " | ") else description
  } else {
    awards_blurb
  }

  full_address <- if (!is.na(street)) {
    paste(c(street, locality), collapse = ", ") |>
      gsub(", NA", "", x = _, fixed = TRUE)
  } else {
    NA_character_
  }

  tibble::tibble(
    name         = decode_html_entities(ld$name %||% NA_character_),
    suburb       = locality,
    address      = full_address,
    cuisine      = NA_character_,
    category     = "Restaurant",
    description  = decode_html_entities(full_blurb),
    price_range  = NA_integer_,
    rating       = NA_real_,
    rating_scale = NA_character_,
    latitude     = NA_real_,
    longitude    = NA_real_,
    url          = url,
    award_year   = award_year
  )
}


#' Pull the first `Restaurant`-typed JSON-LD block out of a detail page
#'
#' W50B emits one ld+json script per detail page (the Restaurant). A
#' broken / non-Restaurant block returns NULL so the caller can skip.
#' @noRd
w50b_extract_jsonld_restaurant <- function(html_str) {
  blocks <- stringr::str_match_all(
    html_str,
    '<script type="application/ld\\+json">([\\s\\S]*?)</script>'
  )[[1]]
  if (nrow(blocks) == 0) return(NULL)
  for (k in seq_len(nrow(blocks))) {
    parsed <- tryCatch(
      jsonlite::fromJSON(blocks[k, 2], simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) next
    if (identical(parsed$`@type`, "Restaurant")) return(parsed)
  }
  NULL
}


#' Pull the highest year out of W50B's award citation array
#'
#' Citation strings include the year (e.g. "The World's 50 Best
#' Restaurants 2025, No. 1"). Returns the most recent year as integer,
#' or `NA_integer_` if none parse.
#' @noRd
w50b_extract_award_year <- function(awards) {
  if (length(awards) == 0) return(NA_integer_)
  years <- stringr::str_match(
    unlist(awards), "(19[5-9][0-9]|20[0-9]{2})"
  )[, 2]
  years <- suppressWarnings(as.integer(years))
  years <- years[!is.na(years)]
  if (length(years) == 0) return(NA_integer_)
  max(years)
}
