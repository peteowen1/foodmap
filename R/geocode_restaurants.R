#' Geocode restaurants via Google Places API
#'
#' For rows missing latitude/longitude, queries the Google Places API (New)
#' Text Search to resolve coordinates. Safe to re-run — skips rows that
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
#'
#' @return The input tibble with `latitude`, `longitude`, `formatted_address`,
#'   and `place_id` columns populated.
#' @export
geocode_restaurants <- function(restaurants,
                                api_key = NULL,
                                cache_path = "cache/geocodes.csv",
                                force_refresh = FALSE) {

  restaurants <- ensure_geocode_cols(restaurants)

  # Step 1 — fill in coordinates from the on-disk cache (unless overridden)
  if (!is.null(cache_path) && !force_refresh && file.exists(cache_path)) {
    restaurants <- geocode_cache_apply(restaurants, cache_path)
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
    query <- build_geocode_query(row$name, row$suburb)

    result <- places_text_search(query, api_key)

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

  # Step 3 — persist the (now expanded) coordinate set to the cache
  if (!is.null(cache_path)) geocode_cache_write(restaurants, cache_path)

  restaurants
}

#' Read a geocode cache CSV and fill matching coords into a restaurants tibble
#' @noRd
geocode_cache_apply <- function(restaurants, cache_path) {
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

  # Self-heal: any cached coords that fall outside Australia get cleared
  # so they'll be re-geocoded with the AU-biased query
  bad <- !is.na(restaurants$latitude) &
    !is_in_australia(restaurants$latitude, restaurants$longitude)
  if (any(bad)) {
    cli::cli_warn(
      "{sum(bad)} cached coord{?s} fell outside Australia and will be re-geocoded"
    )
    restaurants$latitude[bad] <- NA_real_
    restaurants$longitude[bad] <- NA_real_
    if ("formatted_address" %in% names(restaurants))
      restaurants$formatted_address[bad] <- NA_character_
    if ("place_id" %in% names(restaurants))
      restaurants$place_id[bad] <- NA_character_
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
#' @noRd
build_geocode_query <- function(name, suburb) {
  parts <- c(name, suburb, "Australia")
  parts <- parts[!is.na(parts) & nchar(parts) > 0]
  paste(parts, collapse = " ")
}

#' Approximate bounding box of mainland Australia + Tasmania, used to
#' validate cached/returned coordinates and bias Places API queries
#' @noRd
AU_BBOX <- list(lat = c(-44, -10), lng = c(112, 154))

#' Are these coordinates inside the Australia bounding box?
#' @noRd
is_in_australia <- function(lat, lng) {
  !is.na(lat) & !is.na(lng) &
    lat >= AU_BBOX$lat[1] & lat <= AU_BBOX$lat[2] &
    lng >= AU_BBOX$lng[1] & lng <= AU_BBOX$lng[2]
}

#' Call Google Places API (New) Text Search
#'
#' Biases results to Australian places via `regionCode = "AU"` and a
#' `locationBias` rectangle covering Australia. Falls back to the global
#' result if no AU-specific match exists.
#' @noRd
places_text_search <- function(query, api_key) {

  body <- list(
    textQuery   = query,
    regionCode  = "AU",
    locationBias = list(
      rectangle = list(
        low  = list(latitude = AU_BBOX$lat[1], longitude = AU_BBOX$lng[1]),
        high = list(latitude = AU_BBOX$lat[2], longitude = AU_BBOX$lng[2])
      )
    )
  )

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

  # Reject any non-AU results that slipped through (region bias is a
  # preference, not a hard restriction)
  for (p in places) {
    lat <- p$location$latitude
    lng <- p$location$longitude
    if (is_in_australia(lat, lng)) {
      return(list(
        lat      = lat,
        lng      = lng,
        address  = p$formattedAddress %||% NA_character_,
        place_id = p$id %||% NA_character_
      ))
    }
  }

  cli::cli_warn("No Australian results for {.val {query}}")
  NULL
}
