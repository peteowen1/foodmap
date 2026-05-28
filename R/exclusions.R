#' Apply manually-curated venue exclusions
#'
#' Reads `inst/extdata/manual_excludes.csv` and drops any rows matching
#' the curated exclusion records. Use this to remove venues that are
#' stale (closed, renamed), wrongly scraped (cross-region listings),
#' or otherwise problematic - without having to patch the upstream
#' scraper.
#'
#' Matching rules:
#' * `name` is required and matched case-insensitively.
#' * `city` is required. Pass `"*"` to apply across every city.
#' * `suburb` is optional - when present, both name and suburb must
#'   match (also case-insensitive). Leave blank to match the name
#'   regardless of suburb (useful for venues that appear with various
#'   suburbs across guides).
#' * `source` is optional - when present, restricts the exclusion to
#'   rows from that source. Useful when a venue should drop from one
#'   scraper's pull but not others.
#'
#' The exclusion CSV has columns: `city`, `name`, `suburb`, `source`,
#' `reason`, `date_added`. The `reason` and `date_added` columns are
#' for the maintainer; they have no effect on matching.
#'
#' @param restaurants Tibble with at least `name` and (typically)
#'   `suburb` and `source` columns.
#' @param city Character or `NULL`. The current city slug; matches
#'   rows whose `city` value equals this or `"*"`. When `NULL`, only
#'   `"*"` (global) exclusions apply.
#' @return The input tibble with matching rows removed.
#' @export
apply_manual_excludes <- function(restaurants, city = NULL) {
  exclude_path <- system.file("extdata", "manual_excludes.csv",
                              package = "foodmap")
  if (!nzchar(exclude_path) || !file.exists(exclude_path)) {
    return(restaurants)
  }
  excludes <- tryCatch(
    utils::read.csv(exclude_path, stringsAsFactors = FALSE,
                    na.strings = c("", "NA")),
    error = function(e) NULL
  )
  if (is.null(excludes) || nrow(excludes) == 0) return(restaurants)
  required <- c("city", "name")
  if (!all(required %in% names(excludes))) {
    cli::cli_warn(
      "manual_excludes.csv missing required columns; skipping"
    )
    return(restaurants)
  }

  # Scope to global exclusions (city = "*") plus the current city.
  scope <- excludes$city == "*"
  if (!is.null(city)) scope <- scope | excludes$city == city
  excludes <- excludes[scope, , drop = FALSE]
  if (nrow(excludes) == 0) return(restaurants)

  name_lc <- tolower(restaurants$name)
  suburb_lc <- if ("suburb" %in% names(restaurants)) {
    tolower(restaurants$suburb)
  } else {
    rep(NA_character_, nrow(restaurants))
  }
  source_lc <- if ("source" %in% names(restaurants)) {
    tolower(restaurants$source)
  } else {
    rep(NA_character_, nrow(restaurants))
  }

  drop <- rep(FALSE, nrow(restaurants))
  for (i in seq_len(nrow(excludes))) {
    name_match <- !is.na(name_lc) & name_lc == tolower(excludes$name[i])

    if ("suburb" %in% names(excludes) &&
        !is.na(excludes$suburb[i]) && nzchar(excludes$suburb[i])) {
      suburb_match <- !is.na(suburb_lc) &
        suburb_lc == tolower(excludes$suburb[i])
    } else {
      suburb_match <- rep(TRUE, nrow(restaurants))
    }

    if ("source" %in% names(excludes) &&
        !is.na(excludes$source[i]) && nzchar(excludes$source[i])) {
      # source can be a comma-joined list after dedup (e.g.
      # "honolulu_magazine, hale_aina"); match if the exclude's
      # source token appears anywhere in the row's source list.
      pat <- paste0("(^|,\\s*)", tolower(excludes$source[i]),
                    "(,|$)")
      source_match <- !is.na(source_lc) &
        grepl(pat, source_lc, perl = TRUE)
    } else {
      source_match <- rep(TRUE, nrow(restaurants))
    }

    drop <- drop | (name_match & suburb_match & source_match)
  }

  if (any(drop)) {
    n <- sum(drop)
    cli::cli_alert_info(
      "Manual excludes: dropping {n} venue{?s}"
    )
    restaurants <- restaurants[!drop, , drop = FALSE]
  }
  restaurants
}
