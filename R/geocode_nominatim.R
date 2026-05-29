# Geocoding backend: OpenStreetMap Nominatim ---------------------------------
#
# Free public endpoint at https://nominatim.openstreetmap.org. Same return
# contract as places_text_search() in geocode_restaurants.R so the dispatcher
# in geocode_restaurants() doesn't care which backend filled the row.
#
# Two non-negotiable politeness rules from Nominatim's usage policy:
#   1. Max 1 request per second to the public endpoint (caller enforces).
#   2. Identifying User-Agent header - anonymous clients are banned.
#
# Coverage trade-off vs Google Places: Nominatim is strong on street
# addresses and weak on restaurant POIs by name. A query like "Tartine
# Bakery San Francisco" matches reliably; "South End Newtown" much less so.
# We deliberately never fall back to a paid backend - the whole point of
# this backend is "no surprise bills".


#' Geocode via the OpenStreetMap Nominatim public endpoint
#'
#' @return list(lat, lng, address, place_id, neighborhood) or NULL when
#'   nothing inside the target region matched. Never throws on miss.
#' @noRd
nominatim_search <- function(query, country = "AU", city = NULL,
                             user_agent = nominatim_user_agent()) {

  params <- build_nominatim_params(query, country = country, city = city)

  resp <- tryCatch(
    httr2::request("https://nominatim.openstreetmap.org/search") |>
      httr2::req_user_agent(user_agent) |>
      httr2::req_url_query(!!!params) |>
      httr2::req_retry(max_tries = 2) |>
      httr2::req_perform(),
    error = function(e) {
      cli::cli_warn("Nominatim error for {.val {query}}: {e$message}")
      return(NULL)
    }
  )
  if (is.null(resp)) return(NULL)

  results <- httr2::resp_body_json(resp)
  if (length(results) == 0) return(NULL)

  pick <- pick_nominatim_result(results, country = country, city = city)
  if (is.null(pick)) return(NULL)

  list(
    lat          = as.numeric(pick$lat),
    lng          = as.numeric(pick$lon),
    address      = pick$display_name %||% NA_character_,
    place_id     = nominatim_place_id(pick),
    neighborhood = neighborhood_from_nominatim(pick$address)
  )
}


#' Build the query-string parameters for Nominatim /search
#'
#' `countrycodes=` is a HARD restriction (unlike Google's regionCode which
#' is only a bias), so we drop it when country is unknown rather than send
#' an empty value (which Nominatim interprets as "anywhere").
#' @noRd
build_nominatim_params <- function(query, country = NULL, city = NULL) {
  params <- list(
    q              = query,
    format         = "jsonv2",
    addressdetails = 1L,
    limit          = 5L
  )
  cc <- if (!is.null(country) && !is.na(country)) tolower(country) else NULL
  if (!is.null(cc) && nzchar(cc)) params$countrycodes <- cc

  # viewbox + bounded biases results into a rectangle. Nominatim's viewbox
  # is (left, top, right, bottom) = (W lng, N lat, E lng, S lat).
  bbox <- city_bbox(city) %||% country_bbox(country)
  if (!is.null(bbox)) {
    params$viewbox <- sprintf("%f,%f,%f,%f",
                              bbox$lng[1], bbox$lat[2],
                              bbox$lng[2], bbox$lat[1])
    # bounded=1 turns the viewbox into a hard filter at the API level for
    # city-tight searches; we still re-check in pick_nominatim_result()
    # because country-wide viewboxes are too loose to trust alone.
    if (!is.null(city_bbox(city))) params$bounded <- 1L
  }
  params
}


#' Choose the best Nominatim hit from a result list
#'
#' Nominatim returns up to `limit` candidates sorted by `importance`. The
#' first result is usually right, but two corrections matter:
#'
#'   1. Reject anything outside the target region (city bbox if registered,
#'      else country bbox). region bias is "preferred not guaranteed" in
#'      Nominatim too, especially when bounded=1 yields 0 results and the
#'      caller silently widens.
#'   2. Prefer venue-shaped hits (`class == "amenity"`, `type %in% c("restaurant",
#'      "cafe", "bar", "pub", "fast_food", "food_court")`) over the
#'      generic street/building match at the same importance. A query like
#'      "Aria 1 Macquarie Street Sydney" can rank a parking lot first if
#'      that lot has higher OSM importance than the venue node.
#' @noRd
pick_nominatim_result <- function(results, country = NULL, city = NULL) {
  food_types <- c("restaurant", "cafe", "bar", "pub",
                  "fast_food", "food_court", "ice_cream", "biergarten")

  in_region <- function(lat, lng) {
    if (!is.null(city_bbox(city))) is_in_city(lat, lng, city)
    else if (!is.null(country))    is_in_country(lat, lng, country)
    else                           TRUE
  }

  in_region_hits <- Filter(function(r) {
    lat <- suppressWarnings(as.numeric(r$lat))
    lng <- suppressWarnings(as.numeric(r$lon))
    !is.na(lat) && !is.na(lng) && isTRUE(in_region(lat, lng))
  }, results)
  if (length(in_region_hits) == 0) return(NULL)

  amenity_hits <- Filter(function(r) {
    isTRUE(r$class == "amenity") && isTRUE(r$type %in% food_types)
  }, in_region_hits)
  if (length(amenity_hits) > 0) return(amenity_hits[[1]])

  in_region_hits[[1]]
}


#' Compose the durable OSM identifier as `<type>/<id>` (e.g. "node/12345")
#'
#' Mirrors the OSM URL convention. `osm_id` alone isn't enough because the
#' same numeric id can exist on a node, way and relation simultaneously.
#' @noRd
nominatim_place_id <- function(hit) {
  if (!is.null(hit$osm_type) && !is.null(hit$osm_id)) {
    paste0(hit$osm_type, "/", hit$osm_id)
  } else {
    NA_character_
  }
}


#' Extract a neighborhood-equivalent string from Nominatim's address block
#'
#' Mirrors the semantics of neighborhood_from_components() for Google:
#' returns the most specific tag available, with `""` as the "tried, none
#' found" sentinel (NA means "never tried", which migrate_neighborhoods
#' uses to decide what to re-geocode).
#'
#' Skips `suburb` deliberately - it's already a separate column on the
#' restaurant tibble, and storing it in `neighborhood` too would let it
#' display as redundant text in popups.
#' @noRd
neighborhood_from_nominatim <- function(addr) {
  empty_sentinel <- ""
  if (!is.list(addr) || length(addr) == 0) return(empty_sentinel)
  preferred <- c("neighbourhood", "quarter", "city_district", "borough")
  for (want in preferred) {
    val <- addr[[want]]
    if (!is.null(val) && is.character(val) && nzchar(val)) return(val)
  }
  empty_sentinel
}


#' Required User-Agent for Nominatim's public endpoint
#'
#' Their usage policy explicitly bans anonymous clients. The string must
#' identify the application; including a contact URL is the norm.
#' @noRd
nominatim_user_agent <- function() {
  "foodmap-r/0.1 (+https://github.com/peteowen1/foodmap)"
}
