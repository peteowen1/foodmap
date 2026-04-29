# Region / country registry --------------------------------------------------
#
# Cities are the unit of dispatch (you scrape "san-francisco" or "sydney"),
# but several pipeline steps benefit from knowing what *country* a city is in:
#
# - Geocoding: pass an AU vs US bounding box and regionCode to the Places
#   API to keep results in the right country.
# - Source selection: each scraper declares the cities it covers via
#   `supported_cities_for_source()`. Together with a city this naturally
#   restricts to that country's sources, but we sometimes want to enumerate
#   "all sources for country X" without naming a city.
#
# This file is intentionally lightweight: a few small lookup tables keyed
# by city or country. Add a new country by extending all three of
# `city_country()`, `country_bbox()`, and `country_region_code()`.


#' Look up the ISO country code for a city
#'
#' Used by the geocoder and the dispatcher to bias Places API queries and
#' pick the right scraper set. Returns `NA_character_` for unknown cities
#' so callers can decide whether to default or abort.
#'
#' @param city Character. Lowercase city slug (e.g. `"sydney"`,
#'   `"san-francisco"`). Common aliases (`"sf"`, `"nyc"`, `"la"`) are
#'   accepted.
#' @return Two-letter ISO country code, or `NA_character_`.
#' @noRd
city_country <- function(city) {
  city <- tolower(city)
  city <- switch(city,
    sf  = "san-francisco",
    nyc = "new-york",
    la  = "los-angeles",
    city
  )

  # Only cities with at least one supported scraper are listed.
  # When a new city ships with its first scraper, add it here so
  # geocoding picks the right country bias.
  au_cities <- c("sydney", "melbourne", "brisbane", "adelaide", "perth",
                 "hobart", "canberra", "darwin", "gold-coast")
  us_cities <- c("san-francisco")
  uk_cities <- character()

  if (city %in% au_cities) return("AU")
  if (city %in% us_cities) return("US")
  if (length(uk_cities) > 0 && city %in% uk_cities) return("GB")
  NA_character_
}


#' Approximate bounding box for a country
#'
#' Used to bias Places API queries (`locationBias`) and to validate that
#' returned coordinates haven't slipped to the wrong country. Generous
#' enough to include the country's territories; tight enough to catch
#' cross-ocean miss-matches like a US business named "Yan".
#'
#' @param country Two-letter ISO code or `NULL`.
#' @return List with `lat = c(min, max)` and `lng = c(min, max)`, or `NULL`
#'   for an unknown country (no bias applied).
#' @noRd
country_bbox <- function(country) {
  if (is.null(country) || is.na(country)) return(NULL)
  switch(country,
    AU = list(lat = c(-44, -10), lng = c(112, 154)),
    US = list(lat = c(24, 49),   lng = c(-125, -66)),  # continental
    GB = list(lat = c(49, 61),   lng = c(-9, 2)),
    NULL
  )
}


#' Region code passed to Google Places API for ranking bias
#'
#' Maps our two-letter ISO codes to the CLDR region codes the Places API
#' expects. Most of our codes already match.
#' @noRd
country_region_code <- function(country) {
  if (is.null(country) || is.na(country)) return(NULL)
  country
}


#' Are these coordinates inside the given country's bounding box?
#'
#' If the country is unknown / NULL, returns TRUE (no validation possible).
#' Vectorised over `lat` and `lng`.
#' @noRd
is_in_country <- function(lat, lng, country) {
  bbox <- country_bbox(country)
  if (is.null(bbox)) return(rep(TRUE, length(lat)))
  !is.na(lat) & !is.na(lng) &
    lat >= bbox$lat[1] & lat <= bbox$lat[2] &
    lng >= bbox$lng[1] & lng <= bbox$lng[2]
}
