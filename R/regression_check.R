#' Assert the mapped venue count for a city is not a major regression
#'
#' Reads `inst/extdata/expected_counts.csv` and looks up `city`'s
#' `expected_min`. If the supplied `restaurants` tibble has fewer
#' georeferenced venues (rows with non-NA latitude) than that minimum,
#' the function aborts with a clear message naming both numbers.
#'
#' The intent is to catch silent breakage in the upstream sites: a
#' scraper continues to run (no error, no warning) but suddenly
#' returns 50 venues instead of 600. By default this isn't visible -
#' the CSV / KML / HTML still get written. Wiring this assertion into
#' each analysis script makes the pipeline fail loudly when that
#' happens.
#'
#' Expected counts are kept under `inst/extdata/` (not a package
#' constant) so the maintainer can edit them without a code change.
#' The CSV has three columns: `city`, `expected_min`, `note`.
#'
#' @param restaurants Tibble. The geocoded/harmonised pipeline output.
#' @param city Character. City slug used to look up the row.
#' @param expected_counts_path Character or `NULL`. Optional path to a
#'   custom expected-counts CSV. Defaults to `NULL`, which means:
#'   prefer `inst/extdata/expected_counts.csv` in the current working
#'   directory if present (live dev / test fixtures), otherwise fall
#'   back to the installed package's `system.file()` copy.
#' @param require Logical. If `TRUE` (default), missing-city entry
#'   triggers a soft warning and the check is skipped. The pipeline
#'   continues so a city without a baseline isn't blocked from
#'   running.
#'
#' @return The input tibble unchanged (suitable for piping). Aborts
#'   on regression.
#' @export
assert_venue_count <- function(restaurants, city,
                               expected_counts_path = NULL,
                               require = TRUE) {
  exp_path <- if (!is.null(expected_counts_path)) {
    expected_counts_path
  } else {
    # Look in the current working directory first - lets tests shadow
    # the production CSV via withr::local_dir() and matches the dev
    # workflow where analysis/*.R run from the repo root. Only fall
    # back to system.file() when no local copy exists.
    local_path <- file.path("inst", "extdata", "expected_counts.csv")
    if (file.exists(local_path)) local_path
    else system.file("extdata", "expected_counts.csv",
                     package = "foodmap")
  }
  if (!nzchar(exp_path) || !file.exists(exp_path)) {
    if (isTRUE(require)) {
      cli::cli_warn(
        "Expected-counts file not found - skipping regression check."
      )
    }
    return(invisible(restaurants))
  }

  exp <- tryCatch(
    utils::read.csv(exp_path, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (is.null(exp) || !all(c("city", "expected_min") %in% names(exp))) {
    cli::cli_warn("Expected-counts CSV malformed - skipping regression check.")
    return(invisible(restaurants))
  }

  row <- exp[exp$city == city, ]
  if (nrow(row) == 0) {
    if (isTRUE(require)) {
      cli::cli_warn(
        "No expected_min entry for {.val {city}}; skipping regression check."
      )
    }
    return(invisible(restaurants))
  }

  exp_min <- as.integer(row$expected_min[1])
  if (is.na(exp_min)) {
    cli::cli_warn(
      "expected_min for {.val {city}} is not an integer; skipping check."
    )
    return(invisible(restaurants))
  }

  n_geo <- if ("latitude" %in% names(restaurants)) {
    sum(!is.na(restaurants$latitude))
  } else 0L

  if (n_geo < exp_min) {
    cli::cli_abort(c(
      "Regression: {city} mapped {n_geo} venue{?s}, \\
       expected at least {exp_min}.",
      "i" = "Edit {.file inst/extdata/expected_counts.csv} to update the floor \\
             intentionally; otherwise investigate which upstream source broke."
    ))
  }

  cli::cli_alert_success(
    "{city}: {n_geo} mapped venues (floor {exp_min})"
  )
  invisible(restaurants)
}
