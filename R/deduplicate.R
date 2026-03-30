#' Deduplicate restaurants across sources
#'
#' Merges duplicate venues that appear in multiple guides. Matches on
#' normalized name + suburb, then keeps the richest metadata from each copy.
#'
#' @param restaurants A tibble with a `source` column, as returned by
#'   [scrape_all_sources()].
#'
#' @return A tibble with duplicates merged. The `source` column shows all
#'   contributing sources (e.g., "broadsheet, timeout"). A new `n_sources`
#'   column counts how many guides listed the venue.
#' @export
deduplicate_restaurants <- function(restaurants) {
  if (!"source" %in% names(restaurants)) {
    cli::cli_warn("No {.field source} column found, nothing to deduplicate.")
    return(restaurants)
  }

  cli::cli_h2("Deduplicating {nrow(restaurants)} venues")

  # Normalize name for matching
  restaurants$name_norm <- restaurants$name |>
    tolower() |>
    stringr::str_remove_all("[^a-z0-9 ]") |>
    stringr::str_squish()

  # Normalize suburb — strip common suffixes so "Sydney CBD" matches "Sydney"
  restaurants$suburb_norm <- restaurants$suburb |>
    tolower() |>
    stringr::str_squish() |>
    stringr::str_remove("\\s+(cbd|city|central|centre|east|west|north|south)$")

  restaurants$suburb_norm[is.na(restaurants$suburb) |
                          restaurants$suburb_norm == ""] <- NA_character_

  # Group by normalized name + suburb (treat NA suburb as wildcard)
  # Build groups manually to handle the NA-suburb wildcard matching
  groups <- list()
  assigned <- rep(FALSE, nrow(restaurants))

  for (i in seq_len(nrow(restaurants))) {
    if (assigned[i]) next

    name_i <- restaurants$name_norm[i]
    suburb_i <- restaurants$suburb_norm[i]

    # Find all matches: names must be identical, suburbs must match or be NA
    name_match <- restaurants$name_norm == name_i
    suburb_match <- is.na(restaurants$suburb_norm) | is.na(suburb_i) |
      restaurants$suburb_norm == suburb_i
    matches <- which(name_match & suburb_match & !assigned)

    assigned[matches] <- TRUE
    groups <- c(groups, list(matches))
  }

  # Merge each group
  merged <- purrr::map(groups, function(idx) {
    if (length(idx) == 1) {
      row <- restaurants[idx, ]
      row$n_sources <- 1L
      return(row)
    }

    subset <- restaurants[idx, ]

    # Count non-NA values per row to find the "richest"
    richness <- apply(subset, 1, function(r) sum(!is.na(r)))
    best_idx <- which.max(richness)
    merged_row <- subset[best_idx, ]

    # Fill NA values from other rows
    for (col in names(merged_row)) {
      if (col %in% c("source", "name_norm", "suburb_norm", "n_sources")) next
      if (is.na(merged_row[[col]])) {
        non_na <- subset[[col]][!is.na(subset[[col]])]
        if (length(non_na) > 0) merged_row[[col]] <- non_na[1]
      }
    }

    # Special handling: combine sources
    merged_row$source <- paste(unique(subset$source), collapse = ", ")
    merged_row$n_sources <- length(unique(subset$source))

    # Special handling: prefer longest description
    descs <- subset$description[!is.na(subset$description)]
    if (length(descs) > 0) {
      merged_row$description <- descs[which.max(nchar(descs))]
    }

    # Special handling: prefer non-NA coordinates
    coords <- subset[!is.na(subset$latitude) & !is.na(subset$longitude), ]
    if (nrow(coords) > 0) {
      merged_row$latitude <- coords$latitude[1]
      merged_row$longitude <- coords$longitude[1]
    }

    merged_row
  }) |>
    dplyr::bind_rows()

  # Clean up working columns
  merged$name_norm <- NULL
  merged$suburb_norm <- NULL

  n_removed <- nrow(restaurants) - nrow(merged)
  n_multi <- sum(merged$n_sources > 1)
  cli::cli_alert_success(
    "Deduplicated: {nrow(restaurants)} \u2192 {nrow(merged)} venues ({n_removed} duplicates merged, {n_multi} venue{?s} in multiple guides)"
  )

  merged
}
