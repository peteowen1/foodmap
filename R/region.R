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
  us_cities <- c("san-francisco", "honolulu", "new-york", "los-angeles")
  uk_cities <- c("london")

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


#' Drive-time reach bounding box for a city
#'
#' Defines how far out from a city the map should reach. Venues that
#' geocode inside this box are kept; venues outside it are rejected and
#' left without coordinates (so they drop off the KML/HTML map). The box
#' is also handed to the Places API as `locationBias` and still acts as
#' a coarse disambiguation guard - a Sydney query can't resolve to the
#' US, an SF query can't resolve to NYC.
#'
#' These were originally tight *metro* boxes whose only job was
#' disambiguation. They've since been widened to roughly a few hours'
#' drive of the city so regional venues the city guides legitimately
#' list (the NSW South Coast, the Great Ocean Road, Napa / Monterey)
#' survive geocoding instead of being silently dropped. Returns NULL if
#' no box is registered; callers fall back to the country bbox.
#'
#' Note: a rectangle over-reaches at the corners (its diagonal extent
#' exceeds the intended drive radius), but venue data is sparse enough
#' that this is harmless in practice.
#' @noRd
city_bbox <- function(city) {
  if (is.null(city) || is.na(city)) return(NULL)
  city <- tolower(city)
  city <- switch(city,
    sf  = "san-francisco",
    nyc = "new-york",
    la  = "los-angeles",
    city
  )
  switch(city,
    # San Francisco + ~2h drive: south to Santa Cruz / Monterey, north
    # to Napa / Sonoma / Healdsburg, east to Sacramento. Kept tighter
    # than the AU cities because the Bay Area is ringed by other metros.
    `san-francisco` = list(lat = c(36.40, 38.90), lng = c(-123.30, -121.00)),
    # Sydney + ~4h drive: south down the coast past Ulladulla to
    # Bermagui, north to Port Macquarie, inland west to Orange / Mudgee.
    sydney          = list(lat = c(-36.60, -31.00), lng = c(148.50, 153.20)),
    # Melbourne + ~4h drive: the Mornington Peninsula and Great Ocean
    # Road south, Bendigo / Echuca north, east into Gippsland.
    melbourne       = list(lat = c(-39.00, -35.80), lng = c(142.00, 148.50)),
    # Honolulu / O'ahu island. Tight around O'ahu only - guides
    # routinely shout out Maui / Big Island / Kaua'i venues (e.g.
    # Mama's Fish House, Merriman's, The Beach House) that aren't
    # day-trippable, so we let the bbox filter them out instead of
    # pretending the map covers all of Hawai'i. O'ahu spans roughly
    # 21.25-21.75 N / -158.30 to -157.65 W; we widen slightly to keep
    # the windward / North Shore venues comfortably inside.
    honolulu        = list(lat = c(21.20, 21.78), lng = c(-158.35, -157.60)),
    # NYC five boroughs + immediate commute (Long Island City, Jersey
    # City). Wide enough that any of the Michelin Westchester /
    # Hudson Valley satellites get filtered out (they belong to a
    # different sub-region), but loose enough to catch Coney Island
    # to the south and Riverdale / Yonkers to the north.
    `new-york`      = list(lat = c(40.49, 40.92), lng = c(-74.28, -73.68)),
    # Los Angeles metro - covers LA proper, Santa Monica, Venice,
    # Pasadena, Culver City, Beverly Hills, Hollywood, the South Bay,
    # plus into San Gabriel Valley (Din Tai Fung etc.). Doesn't reach
    # Long Beach or Orange County by design - those have their own
    # local guides and dominate any LA scrape if we don't bound.
    `los-angeles`   = list(lat = c(33.85, 34.30), lng = c(-118.70, -118.10)),
    # Greater London, M25-ish. Catches Zone 1-6 plus the satellites
    # the food guides routinely cover (Hampstead, Greenwich, Ealing,
    # Wimbledon). Tight enough to filter out Brighton / Oxford / Bath
    # picks that occasionally leak into "best UK restaurants" guides.
    london          = list(lat = c(51.28, 51.69), lng = c(-0.51, 0.33)),
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


#' Are these coordinates inside the given city's bounding box?
#'
#' Like `is_in_country()` but tighter. If no bbox is registered for the
#' city, returns TRUE for all coords (no validation possible).
#' @noRd
is_in_city <- function(lat, lng, city) {
  bbox <- city_bbox(city)
  if (is.null(bbox)) return(rep(TRUE, length(lat)))
  !is.na(lat) & !is.na(lng) &
    lat >= bbox$lat[1] & lat <= bbox$lat[2] &
    lng >= bbox$lng[1] & lng <= bbox$lng[2]
}
