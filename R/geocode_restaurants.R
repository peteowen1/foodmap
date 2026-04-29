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
#'   coordinates outside the country's bounding box are rejected. Set
#'   `NULL` for an unbiased global query. Default `"AU"` for back-compat
#'   with the original Sydney/Melbourne pipelines; pass `"US"`, `"GB"`,
#'   etc. for other regions.
#'
#' @return The input tibble with `latitude`, `longitude`, `formatted_address`,
#'   and `place_id` columns populated.
#' @export
geocode_restaurants <- function(restaurants,
                                api_key = NULL,
                                cache_path = "cache/geocodes.csv",
                                force_refresh = FALSE,
                                country = "AU") {

  restaurants <- ensure_geocode_cols(restaurants)

  # Step 1 -- fill in coordinates from the on-disk cache (unless overridden)
  if (!is.null(cache_path) && !force_refresh && file.exists(cache_path)) {
    restaurants <- geocode_cache_apply(restaurants, cache_path, country)
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
    query <- build_geocode_query(row$name, row$suburb, row$address, country)

    result <- places_text_search(query, api_key, country = country)

    if (!is.null(result)) {
      restaurants$latitude[i]          <- result$lat
      restaurants$longitude[i]         <- result$lng
      restaurants$formatted_address[i] <- result$address
      restaurants$place_id[i]          <- result$place_id
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

  # Step 3 -- persist the (now expanded) coordinate set to the cache
  if (!is.null(cache_path)) geocode_cache_write(restaurants, cache_path)

  restaurants
}

#' Read a geocode cache CSV and fill matching coords into a restaurants tibble
#' @noRd
geocode_cache_apply <- function(restaurants, cache_path, country = NULL) {
  cached <- tryCatch(
    utils::read.csv(cache_path, stringsAsFactors = FALSE,
                    na.strings = c("", "NA")),
    error = function(e) NULL
  )
  required <- c("name", "suburb", "latitude", "longitude")
  if (is.null(cached) || !all(required %in% names(cached))) {
    return(restaurants)
  }
  cache_cols <- intersect(
    names(cached),
    c("name", "suburb", "latitude", "longitude", "formatted_address", "place_id")
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
  # bounding box get cleared so they'll be re-geocoded with the right
  # regional bias. Skipped when country is NULL (no bias was applied).
  if (!is.null(country) && !is.na(country)) {
    bad <- !is.na(restaurants$latitude) &
      !is_in_country(restaurants$latitude, restaurants$longitude, country)
    if (any(bad)) {
      cli::cli_warn(
        "{sum(bad)} cached coord{?s} fell outside {country} and will be re-geocoded"
      )
      restaurants$latitude[bad] <- NA_real_
      restaurants$longitude[bad] <- NA_real_
      if ("formatted_address" %in% names(restaurants))
        restaurants$formatted_address[bad] <- NA_character_
      if ("place_id" %in% names(restaurants))
        restaurants$place_id[bad] <- NA_character_
    }
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
  rows <- restaurants[
    !is.na(restaurants$latitude) & !is.na(restaurants$longitude),
    c("name", "suburb", "latitude", "longitude",
      "formatted_address", "place_id")
  ]
  if (nrow(rows) == 0) return(invisible(NULL))
  rows <- rows[!duplicated(rows[, c("name", "suburb")]), , drop = FALSE]

  if (file.exists(cache_path)) {
    existing <- tryCatch(
      utils::read.csv(cache_path, stringsAsFactors = FALSE,
                      na.strings = c("", "NA")),
      error = function(e) NULL
    )
    if (!is.null(existing) && all(c("name", "suburb") %in% names(existing))) {
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
#' The country *name* is appended to the query text (in addition to the
#' API-side `regionCode`/bbox bias) because Places gives noticeable
#' weight to the textual signal. Without it, an SF venue's query like
#' "Tartine 600 Guerrero St San Francisco" can lose to a same-named AU
#' venue, even with a US bbox set, because the text doesn't disambiguate.
#' @noRd
build_geocode_query <- function(name, suburb, address = NA_character_,
                                country = "AU") {
  parts <- c(name, address, suburb, country_query_label(country))
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
#' rectangle covering the country's bbox). When `country` is `NULL`,
#' falls back to a globally-unbiased query. After the API responds,
#' walks the returned places and rejects anything outside the country's
#' bbox so a strongly-matched foreign place can't slip through the
#' soft `regionCode` bias.
#' @noRd
places_text_search <- function(query, api_key, country = "AU") {

  body <- list(textQuery = query)
  rcode <- country_region_code(country)
  bbox  <- country_bbox(country)
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
        `X-Goog-FieldMask` = "places.location,places.formattedAddress,places.id"
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

  if (length(places) == 0) {
    cli::cli_warn("No results for {.val {query}}")
    return(NULL)
  }

  # Reject any results outside the target country's bbox (region bias
  # is a preference, not a hard restriction). When country is NULL,
  # is_in_country returns TRUE for everything so the first hit wins.
  for (p in places) {
    lat <- p$location$latitude
    lng <- p$location$longitude
    if (isTRUE(is_in_country(lat, lng, country))) {
      return(list(
        lat      = lat,
        lng      = lng,
        address  = p$formattedAddress %||% NA_character_,
        place_id = p$id %||% NA_character_
      ))
    }
  }

  cli::cli_warn("No {country %||% 'matching'} results for {.val {query}}")
  NULL
}
