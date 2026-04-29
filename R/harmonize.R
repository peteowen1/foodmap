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

  # Synthesise n_sources when absent (e.g. caller passed raw scraper
  # output, not a deduped tibble). Done up front so any consumer that
  # reads the harmonized output - export_html(), filters, downstream
  # analytics - can rely on the column existing.
  if (!"n_sources" %in% names(data)) {
    data$n_sources <- vapply(
      strsplit(data$source %||% NA_character_, ",\\s*"),
      function(parts) length(parts[!is.na(parts) & nzchar(parts)]),
      integer(1)
    )
  }

  data |>
    # Fill source-specific extras to NA when absent so case_when can
    # reference them safely below.
    dplyr::mutate(
      hats = if ("hats" %in% names(data)) .data$hats else NA_real_,
      cost_range = if ("cost_range" %in% names(data)) .data$cost_range else NA_character_,
      review_date = if ("review_date" %in% names(data)) .data$review_date else NA_character_,
      michelin_distinction = if ("michelin_distinction" %in% names(data)) .data$michelin_distinction else NA_character_,
      price_inferred = if ("price_inferred" %in% names(data)) .data$price_inferred else FALSE
    ) |>
    # Build human-readable labels. price_label gets a "(est.)" suffix
    # when the price came from rule-based inference rather than a guide,
    # so popup readers can spot which prices are derived.
    dplyr::mutate(
      price_label = dplyr::case_when(
        is.na(.data$price_range) ~ NA_character_,
        .data$price_inferred ~ paste0(format_price_label(.data$price_range), " (est.)"),
        TRUE ~ format_price_label(.data$price_range)
      ),
      cost_bracket = dplyr::case_when(
        !is.na(.data$cost_range) ~ .data$cost_range,
        !is.na(.data$price_range) ~ price_to_bracket(.data$price_range),
        TRUE ~ NA_character_
      ),
      rating_label = dplyr::case_when(
        # Michelin distinction wins when present — it's the recognisable
        # award for those venues, even if they also got a /20 from GFG.
        !is.na(.data$michelin_distinction) ~ paste0("Michelin: ", .data$michelin_distinction),
        TRUE ~ format_rating_label(
          .data$rating, .data$rating_scale, .data$hats, .data$source
        )
      )
    ) |>
    # Select final columns in a useful order. `n_sources` is added by
    # deduplicate_restaurants() and is preserved here so downstream
    # consumers (export_html() in particular) can classify venues by
    # cross-guide overlap.
    dplyr::select(
      "name", "suburb", "address", "cuisine", "category", "source",
      "description", "url", "latitude", "longitude",
      "price_range", "price_label", "cost_bracket", "price_inferred",
      "rating", "rating_scale", "rating_label",
      "hats", "review_date",
      dplyr::any_of(c("n_sources", "michelin_distinction", "michelin_year"))
    )
}


#' Format price_range integer (1-5) as dollar signs, NA-preserving
#'
#' Tibble-friendly counterpart of `format_price()` in utils.R. Same
#' shape, different NA convention: returns `NA_character_` so
#' downstream `dplyr::case_when()` chains can keep missing prices
#' missing rather than coercing them to `""`.
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
