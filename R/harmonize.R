#' Harmonize restaurant data across sources into a clean union
#'
#' Takes the raw combined output from [scrape_all_sources()] and maps
#' source-specific columns into a consistent schema. Each venue-source
#' combination is one row (no deduplication).
#'
#' Adds human-readable labels for ratings and prices, and folds
#' source-specific extras (GFG hats, cost_range, review_date) into
#' the unified columns.
#'
#' @param data A tibble from `scrape_all_sources()` or `dplyr::bind_rows()`
#'   of individual scraper outputs. Must have a `source` column.
#'
#' @return A tibble with columns: name, suburb, address, cuisine, category,
#'   source, description, url, latitude, longitude, price_range, price_label,
#'   cost_bracket, rating, rating_scale, rating_label, hats, review_date.
#' @export
harmonize_sources <- function(data) {
  if (!"source" %in% names(data)) {
    cli::cli_abort("{.arg data} must have a {.field source} column.")
  }

  data |>
    # Fill GFG hats into rating where rating is missing
    dplyr::mutate(
      hats = if ("hats" %in% names(data)) .data$hats else NA_real_,
      cost_range = if ("cost_range" %in% names(data)) .data$cost_range else NA_character_,
      review_date = if ("review_date" %in% names(data)) .data$review_date else NA_character_
    ) |>
    # Build human-readable labels
    dplyr::mutate(
      price_label = format_price_label(.data$price_range),
      cost_bracket = dplyr::case_when(
        !is.na(.data$cost_range) ~ .data$cost_range,
        !is.na(.data$price_range) ~ price_to_bracket(.data$price_range),
        TRUE ~ NA_character_
      ),
      rating_label = format_rating_label(
        .data$rating, .data$rating_scale, .data$hats, .data$source
      )
    ) |>
    # Select final columns in a useful order
    dplyr::select(
      "name", "suburb", "address", "cuisine", "category", "source",
      "description", "url", "latitude", "longitude",
      "price_range", "price_label", "cost_bracket",
      "rating", "rating_scale", "rating_label",
      "hats", "review_date"
    )
}


#' Format price_range integer (1-5) as dollar signs
#' @noRd
format_price_label <- function(price_range) {
  dplyr::case_when(
    is.na(price_range) | price_range == 0 ~ NA_character_,
    TRUE ~ strrep("$", price_range)
  )
}

#' Map price_range integer to approximate cost bracket
#' @noRd
price_to_bracket <- function(price_range) {
  dplyr::case_when(
    price_range == 1 ~ "Under $30",
    price_range == 2 ~ "$30-$60",
    price_range == 3 ~ "$60-$100",
    price_range == 4 ~ "$100-$200",
    price_range == 5 ~ "$200+",
    TRUE ~ NA_character_
  )
}

#' Format a human-readable rating label from source-specific values
#' @noRd
format_rating_label <- function(rating, rating_scale, hats, source) {
  dplyr::case_when(
    # GFG hats take priority as a recognizable award
    !is.na(hats) & hats > 0 ~ paste0(hats, " hat", ifelse(hats > 1, "s", "")),
    # GFG score out of 20
    !is.na(rating_scale) & rating_scale == "gfg_score_20" ~
      paste0(rating, "/20"),
    # Time Out stars out of 5
    !is.na(rating_scale) & rating_scale == "timeout_stars_5" ~
      paste0(rating, "/5 stars"),
    # AGFG award score out of 19
    !is.na(rating_scale) & rating_scale == "agfg_hats_19" ~
      paste0(rating, "/19"),
    TRUE ~ NA_character_
  )
}
