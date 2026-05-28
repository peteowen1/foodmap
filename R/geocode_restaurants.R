#' Geocode restaurants via Google Places API
#'
#' For rows missing latitude/longitude, queries the Google Places API (New)
#' Text Search to resolve coordinates. Safe to re-run -- skips rows that
#' already have coordinates and reuses prior results from a local cache.
#'
#' @param restaurants A tibble as returned by [scrape_broadsheet()].
#' @param api_key Character. Google Places API key. Defaults to the
#'   `GOOGLE_PLACES_API_KEY` environment variable.
#' @param cache_path Character or `NULL`. Path to a CSV file used to cache
#'   geocoded coordinates by `(name, suburb)` between runs. The cache is
#'   read before any API calls (filling in matching rows missing coords)
#'   and updated after geocoding. Default `"cache/geocodes.csv"`.
#'   Pass `NULL` to disable caching entirely.
#' @param force_refresh Logical. If `TRUE`, ignore the cache when reading
#'   (so all rows missing coords get re-geocoded), but still write fresh
#'   results back to the cache. Useful when upstream data has changed and
#'   you suspect cached coordinates are stale. Default `FALSE`.
#' @param country Two-letter ISO country code used to bias Places API
#'   results (`regionCode` + bounding box `locationBias`). Returned
#'   coordinates outside the country's bounding box are rejected. If
#'   left at the default (`NULL`) and `city` is supplied, the country
#'   is inferred from `city_country(city)` - this is almost always what
#'   you want. Falls back to `"AU"` when neither is set, for back-compat
#'   with the original Sydney/Melbourne-only pipelines.
#' @param city Character or `NULL`. Optional city slug (e.g.
#'   `"san-francisco"`). When the city has a registered tight bbox in
#'   `city_bbox()`, that's used both as the API `locationBias` and as
#'   the post-validation rectangle - much stricter than the country
#'   bbox, so same-named venues in other cities (Sai's in NYC vs SF)
#'   can't slip through. Defaults to `NULL` (country-only bias).
#' @param migrate_neighborhoods Logical. If `TRUE`, any cache row that
#'   has coordinates but is missing `neighborhood` gets re-geocoded so
#'   the new structured neighborhood field can be filled in. One-time
#'   cost the first time you run after the field was added; subsequent
#'   runs are no-ops because every cached row will have a value (even
#'   if `NA` - meaning Google didn't return one). Default `FALSE`.
#'
#' @return The input tibble with `latitude`, `longitude`,
#'   `formatted_address`, `place_id`, and `neighborhood` columns
#'   populated.
#' @export
geocode_restaurants <- function(restaurants,
                                api_key = NULL,
                                cache_path = "cache/geocodes.csv",
                                force_refresh = FALSE,
                                country = NULL,
                                city = NULL,
                                migrate_neighborhoods = FALSE) {

  # Default country: infer from city when possible, else "AU" for
  # back-compat with the original Sydney/Melbourne pipelines. Without
  # this inference, geocode_restaurants(rows, city = "los-angeles")
  # would validate LA coords against the AU bbox and silently wipe them.
  if (is.null(country)) {
    country <- if (!is.null(city)) city_country(city) %||% "AU" else "AU"
  }

  restaurants <- ensure_geocode_cols(restaurants)

  # Step 1 -- fill in coordinates from the on-disk cache (unless overridden)
  if (!is.null(cache_path) && !force_refresh && file.exists(cache_path)) {
    restaurants <- geocode_cache_apply(restaurants, cache_path, country,
                                       city = city)
  }

  # When migrating, treat any row whose neighborhood is missing as
  # "needs geocoding" even if coords are already filled. Clearing
  # latitude here forces the loop below to re-fetch via the API,
  # picking up the structured addressComponents this time.
  if (isTRUE(migrate_neighborhoods)) {
    if (!"neighborhood" %in% names(restaurants)) {
      restaurants$neighborhood <- NA_character_
    }
    # NA means "never tried"; "" means "tried, Google returned no
    # neighborhood for this venue" (typical for AU venues). Only the
    # NA case is stale - the sentinel is final.
    stale <- !is.na(restaurants$latitude) & is.na(restaurants$neighborhood)
    if (any(stale)) {
      cli::cli_alert_info(
        "Migrating {sum(stale)} cached venue{?s} to capture neighborhood"
      )
      restaurants$latitude[stale]  <- NA_real_
      restaurants$longitude[stale] <- NA_real_
    }
  }

  needs_geocoding <- is.na(restaurants$latitude) | is.na(restaurants$longitude)
  n_todo <- sum(needs_geocoding)

  if (n_todo == 0) {
    cli::cli_alert_success("All {nrow(restaurants)} venues already have coordinates")
    if (!is.null(cache_path)) geocode_cache_write(restaurants, cache_path)
    return(restaurants)
  }

  cli::cli_h2("Geocoding {n_todo} venue{?s} via Google Places API")

  api_key <- resolve_api_key(api_key)

  cli::cli_progress_bar("Geocoding", total = n_todo)
  idx <- which(needs_geocoding)

  for (i in idx) {
    row <- restaurants[i, ]
    query <- build_geocode_query(row$name, row$suburb, row$address,
                                 country, city = city)

    result <- places_text_search(query, api_key, country = country, city = city)

    if (!is.null(result)) {
      restaurants$latitude[i]          <- result$lat
      restaurants$longitude[i]         <- result$lng
      restaurants$formatted_address[i] <- result$address
      restaurants$place_id[i]          <- result$place_id
      restaurants$neighborhood[i]      <- result$neighborhood
    }

    cli::cli_progress_update()
    Sys.sleep(RATE_LIMIT_SECS)
  }

  cli::cli_progress_done()

  n_found <- sum(!is.na(restaurants$latitude[idx]))
  n_missing <- n_todo - n_found
  cli::cli_alert_success("Geocoded {n_found}/{n_todo} venue{?s}")
  if (n_missing > 0) {
    cli::cli_warn("{n_missing} venue{?s} could not be geocoded")
  }

  # Step 3 -- backfill source-NA addresses with the geocoder's
  # formatted address. Sources like Sprudge intentionally leave
  # address blank because they don't publish structured location
  # data; once Google has resolved the venue we have a clean string
  # to use everywhere downstream (CSV, KML popups, future filters).
  if ("address" %in% names(restaurants) &&
      "formatted_address" %in% names(restaurants)) {
    fill <- is.na(restaurants$address) & !is.na(restaurants$formatted_address)
    if (any(fill)) {
      restaurants$address[fill] <- restaurants$formatted_address[fill]
    }
  }

  # Step 4 -- persist the (now expanded) coordinate set to the cache
  if (!is.null(cache_path)) geocode_cache_write(restaurants, cache_path)

  restaurants
}

#' Read a geocode cache CSV and fill matching coords into a restaurants tibble
#' @noRd
geocode_cache_apply <- function(restaurants, cache_path, country = NULL,
                                city = NULL) {
  cached <- tryCatch(
    utils::read.csv(cache_path, stringsAsFactors = FALSE,
                    # Don't conflate "" with NA - we use empty
                    # strings as the "tried-but-empty" sentinel for
                    # neighborhood, distinct from "never tried" (NA).
                    na.strings = "NA"),
    error = function(e) NULL
  )
  required <- c("name", "suburb", "latitude", "longitude")
  if (is.null(cached) || !all(required %in% names(cached))) {
    return(restaurants)
  }
  cache_cols <- intersect(
    names(cached),
    c("name", "suburb", "latitude", "longitude",
      "formatted_address", "place_id", "neighborhood")
  )
  cached <- cached[!is.na(cached$latitude), cache_cols, drop = FALSE]
  cached <- cached[!duplicated(cached[, c("name", "suburb")]), , drop = FALSE]

  before <- sum(!is.na(restaurants$latitude))
  restaurants <- dplyr::rows_update(
    restaurants, cached,
    by = c("name", "suburb"),
    unmatched = "ignore"
  )

  # Self-heal: any cached coords that fall outside the target country's
  # bounding box get cleared so they'll be re-geocoded. We deliberately
  # do NOT enforce the tighter city bbox here - cached venues that
  # geocoded via the country-only fallback (e.g. regional NSW places
  # listed in the SMH "Sydney" Good Food Guide) would otherwise be
  # wiped on every subsequent run, burning API spend. The downstream
  # map exporters apply the city bbox at display time instead.
  invalid <- if (!is.null(country) && !is.na(country)) {
    !is.na(restaurants$latitude) &
      !is_in_country(restaurants$latitude, restaurants$longitude, country)
  } else {
    rep(FALSE, nrow(restaurants))
  }
  if (any(invalid)) {
    cli::cli_warn(
      "{sum(invalid)} cached coord{?s} fell outside {country} and will be re-geocoded"
    )
    restaurants$latitude[invalid] <- NA_real_
    restaurants$longitude[invalid] <- NA_real_
    if ("formatted_address" %in% names(restaurants))
      restaurants$formatted_address[invalid] <- NA_character_
    if ("place_id" %in% names(restaurants))
      restaurants$place_id[invalid] <- NA_character_
  }

  reused <- sum(!is.na(restaurants$latitude)) - before
  if (reused > 0) {
    cli::cli_alert_info(
      "Reused {reused} cached coordinate{?s} from {.file {cache_path}}"
    )
  }
  restaurants
}

#' Persist successfully-geocoded rows to the cache CSV (upserting by name+suburb)
#' @noRd
geocode_cache_write <- function(restaurants, cache_path) {
  cols <- c("name", "suburb", "latitude", "longitude",
            "formatted_address", "place_id", "neighborhood")
  cols <- intersect(cols, names(restaurants))
  rows <- restaurants[
    !is.na(restaurants$latitude) & !is.na(restaurants$longitude),
    cols
  ]
  if (nrow(rows) == 0) return(invisible(NULL))
  rows <- rows[!duplicated(rows[, c("name", "suburb")]), , drop = FALSE]

  if (file.exists(cache_path)) {
    existing <- tryCatch(
      utils::read.csv(cache_path, stringsAsFactors = FALSE,
                      # Don't conflate "" with NA - we use empty
                    # strings as the "tried-but-empty" sentinel for
                    # neighborhood, distinct from "never tried" (NA).
                    na.strings = "NA"),
      error = function(e) NULL
    )
    if (!is.null(existing) && all(c("name", "suburb") %in% names(existing))) {
      # rows_upsert requires y's columns to be a subset of x's. When
      # we add a new field (e.g. neighborhood), the old cache CSV
      # lacks that column - back-fill it as NA before upserting.
      missing_cols <- setdiff(names(rows), names(existing))
      for (mc in missing_cols) {
        existing[[mc]] <- if (is.character(rows[[mc]])) NA_character_
                          else if (is.numeric(rows[[mc]])) NA_real_
                          else NA
      }
      merged <- dplyr::rows_upsert(existing, rows, by = c("name", "suburb"))
    } else {
      merged <- rows
    }
  } else {
    dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
    merged <- rows
  }

  utils::write.csv(merged, cache_path, row.names = FALSE)
  invisible(NULL)
}

#' Ensure geocode output columns exist
#' @noRd
ensure_geocode_cols <- function(df) {
  if (!"formatted_address" %in% names(df)) {
    df$formatted_address <- NA_character_
  }
  if (!"place_id" %in% names(df)) {
    df$place_id <- NA_character_
  }
  if (!"neighborhood" %in% names(df)) {
    df$neighborhood <- NA_character_
  }
  df
}

#' Build a geocoding query string
#'
#' Includes the source-provided address when available, since it's a
#' much stronger signal than name+suburb alone (e.g. "South End Newtown"
#' picks the wrong end of King Street; "South End 644 King Street
#' Erskineville Newtown" picks the right venue). Address is allowed to
#' contradict suburb because guides routinely disagree on which suburb
#' a boundary venue belongs to.
#'
#' When no address is available we additionally append the city's state
#' (NSW / VIC / California / ...) because suburb names alone aren't
#' always globally unique - "Brunswick Heads" is meaningful in NSW but
#' Google can latch onto a Brunswick suburb elsewhere without the
#' state hint. Skipped when an address is present because the postcode
#' already disambiguates.
#'
#' The country *name* is appended to the query text (in addition to the
#' API-side `regionCode`/bbox bias) because Places gives noticeable
#' weight to the textual signal. Without it, an SF venue's query like
#' "Tartine 600 Guerrero St San Francisco" can lose to a same-named AU
#' venue, even with a US bbox set, because the text doesn't disambiguate.
#' @noRd
build_geocode_query <- function(name, suburb, address = NA_character_,
                                country = "AU", city = NULL) {
  parts <- c(name, address, suburb)
  if (is.na(address) || !nzchar(address)) {
    parts <- c(parts, city_state(city))
  }
  parts <- c(parts, country_query_label(country))
  parts <- parts[!is.na(parts) & nchar(parts) > 0]
  paste(parts, collapse = " ")
}

#' Country code -> human-readable name for inclusion in geocode queries
#' @noRd
country_query_label <- function(country) {
  if (is.null(country) || is.na(country)) return(NA_character_)
  switch(country,
    AU = "Australia",
    US = "United States",
    GB = "United Kingdom",
    NA_character_
  )
}

#' Call Google Places API (New) Text Search
#'
#' Biases results to the requested country (regionCode + locationBias
#' rectangle covering the country's bbox), and when `city` is supplied
#' tightens that bias to the city's drive-time bbox so same-name venues
#' in other cities can't outrank the local one.
#'
#' When a city-constrained search returns nothing usable, this falls
#' back to a country-only attempt. The fallback exists for venues that
#' are legitimately listed in a city's food guide but sit outside the
#' city's metro bbox - e.g. Byron Bay restaurants in SMH's "Sydney"
#' Good Food Guide. The downstream consumer (KML/HTML map exporters)
#' is responsible for filtering these back out of city-scoped maps.
#' @noRd
places_text_search <- function(query, api_key, country = "AU", city = NULL) {
  result <- places_search_attempt(query, api_key, country, city)
  if (!is.null(result)) return(result)

  # Retry without the city bbox if there was one to drop. Without this
  # guard the fallback would just repeat the original (country-only)
  # call and waste an API request.
  if (!is.null(city) && !is.null(city_bbox(city))) {
    result <- places_search_attempt(query, api_key, country, city = NULL)
    if (!is.null(result)) return(result)
  }

  region_label <- city %||% country %||% "matching"
  cli::cli_warn("No {region_label} results for {.val {query}}")
  NULL
}

#' One attempt at a Places Text Search. Returns a result list or NULL.
#' No CLI warnings on miss - the wrapper decides whether to retry.
#' @noRd
places_search_attempt <- function(query, api_key, country, city) {

  body <- list(textQuery = query)
  rcode <- country_region_code(country)
  # Prefer the tight city bbox over the country bbox when available -
  # otherwise SF queries get all 9.8M km^2 of the US to choose from
  # and same-named venues in other US cities can outrank the SF one.
  bbox  <- city_bbox(city) %||% country_bbox(country)
  if (!is.null(rcode)) body$regionCode <- rcode
  if (!is.null(bbox)) {
    body$locationBias <- list(
      rectangle = list(
        low  = list(latitude = bbox$lat[1], longitude = bbox$lng[1]),
        high = list(latitude = bbox$lat[2], longitude = bbox$lng[2])
      )
    )
  }

  resp <- tryCatch(
    httr2::request("https://places.googleapis.com/v1/places:searchText") |>
      httr2::req_headers(
        `Content-Type`     = "application/json",
        `X-Goog-Api-Key`   = api_key,
        # addressComponents is free in API quota terms - it just adds
        # more data per response - and gives us the structured
        # neighborhood / sublocality fields the formatted address
        # doesn't include.
        `X-Goog-FieldMask` = "places.location,places.formattedAddress,places.addressComponents,places.id"
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_retry(max_tries = 2) |>
      httr2::req_perform(),
    error = function(e) {
      cli::cli_warn("API error for {.val {query}}: {e$message}")
      return(NULL)
    }
  )

  if (is.null(resp)) return(NULL)

  data <- httr2::resp_body_json(resp)
  places <- data$places

  if (length(places) == 0) return(NULL)

  # Reject any results outside the target bbox (region bias is a
  # preference, not a hard restriction). City bbox wins over country
  # when both are set; when neither is set, accept everything.
  in_region <- function(lat, lng) {
    if (!is.null(city_bbox(city))) is_in_city(lat, lng, city)
    else is_in_country(lat, lng, country)
  }
  for (p in places) {
    lat <- p$location$latitude
    lng <- p$location$longitude
    if (isTRUE(in_region(lat, lng))) {
      return(list(
        lat          = lat,
        lng          = lng,
        address      = p$formattedAddress %||% NA_character_,
        place_id     = p$id %||% NA_character_,
        neighborhood = neighborhood_from_components(p$addressComponents)
      ))
    }
  }

  NULL
}


#' Pull a neighborhood (or sublocality) string out of Google's
#' addressComponents array.
#'
#' Google ranks the components by specificity. We prefer the most
#' granular tag in this order:
#'   1. neighborhood (most specific, e.g. "Hayes Valley")
#'   2. sublocality_level_1 (e.g. "Mission")
#'   3. sublocality (rare; usually duplicates the above)
#'
#' Returns `NA_character_` when none of those types are present in
#' the response (typical for venues outside dense urban areas).
#' @noRd
neighborhood_from_components <- function(components) {
  empty_sentinel <- ""  # See note below
  if (!is.list(components) || length(components) == 0) return(empty_sentinel)
  preferred <- c("neighborhood", "sublocality_level_1", "sublocality")
  for (want in preferred) {
    for (c in components) {
      types <- c$types
      if (is.list(types)) types <- unlist(types)
      if (want %in% types) {
        val <- c$longText %||% c$shortText %||% NA_character_
        if (!is.na(val) && nzchar(val)) return(val)
      }
    }
  }
  # Empty string (not NA) is the "tried, nothing found" sentinel.
  # AU venues never have neighborhood/sublocality components - suburbs
  # are the neighborhood-equivalent there - so without a sentinel
  # every Sydney/Melbourne row would re-trigger
  # migrate_neighborhoods on every subsequent run, wasting API spend.
  # popup_location() and the migrate stale check treat "" as
  # "definitively no neighborhood for this venue".
  empty_sentinel
}
