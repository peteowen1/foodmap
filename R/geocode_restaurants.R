#' Geocode restaurants via Google Places API
#'
#' For rows missing latitude/longitude, queries the Google Places API (New)
#' Text Search to resolve coordinates. Safe to re-run — skips rows that
#' already have coordinates.
#'
#' @param restaurants A tibble as returned by [scrape_broadsheet()].
#' @param api_key Character. Google Places API key. Defaults to the
#'   `GOOGLE_PLACES_API_KEY` environment variable.
#'
#' @return The input tibble with `latitude`, `longitude`, `formatted_address`,
#'   and `place_id` columns populated.
#' @export
geocode_restaurants <- function(restaurants, api_key = NULL) {

  api_key <- resolve_api_key(api_key)

  needs_geocoding <- is.na(restaurants$latitude) | is.na(restaurants$longitude)
  n_todo <- sum(needs_geocoding)

  if (n_todo == 0) {
    cli::cli_alert_success("All {nrow(restaurants)} venues already have coordinates")
    return(ensure_geocode_cols(restaurants))
  }

  cli::cli_h2("Geocoding {n_todo} venue{?s} via Google Places API")

  # Add columns if missing
  restaurants <- ensure_geocode_cols(restaurants)

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

  restaurants
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

#' Call Google Places API (New) Text Search
#' @noRd
places_text_search <- function(query, api_key) {

  body <- list(textQuery = query)

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

  top <- places[[1]]
  list(
    lat      = top$location$latitude,
    lng      = top$location$longitude,
    address  = top$formattedAddress %||% NA_character_,
    place_id = top$id %||% NA_character_
  )
}
