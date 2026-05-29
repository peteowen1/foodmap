#' Export a diagnostics CSV listing rows with missing data
#'
#' Surfaces the rows in a pipeline output that have one or more
#' underspecified fields - typically the ones that fall out of the KML
#' or render as bare popups in the HTML map. Writes a CSV with two
#' extra columns:
#'
#'   * `issues` - comma-separated list of fields that are blank for
#'     this row (e.g. "latitude, description").
#'   * `issue_count` - number of fields blank, used to sort worst-first.
#'
#' Pipe the resulting CSV into manual fix decisions: either add a row
#' to `inst/extdata/manual_excludes.csv`, or correct the cache entry
#' directly, or open a scraper-improvement issue.
#'
#' @param restaurants A tibble as returned by `harmonize_sources()` or
#'   any pipeline stage with the standard schema.
#' @param output_path Character. File path for the diagnostics CSV.
#' @param fields Character vector of column names to consider "key".
#'   Default `c("latitude", "address", "description", "price_range",
#'   "cuisine", "url")`. Pass a subset to focus on a single class of
#'   issue (e.g. only geocoding misses).
#'
#' @return The output path (invisibly), or `NULL` when there are no
#'   issues to report.
#' @export
export_diagnostics <- function(restaurants, output_path,
                               fields = c("latitude", "address",
                                          "description", "price_range",
                                          "cuisine", "url")) {
  present <- intersect(fields, names(restaurants))
  if (length(present) == 0) {
    cli::cli_warn(
      "None of the requested diagnostic fields exist in the tibble; \\
       nothing to write."
    )
    return(invisible(NULL))
  }

  blank_matrix <- vapply(
    present,
    function(f) is_blank_field(restaurants[[f]]),
    logical(nrow(restaurants))
  )
  # vapply returns a vector when nrow == 1; force a matrix shape so the
  # rowSums / apply calls below work uniformly.
  if (!is.matrix(blank_matrix)) {
    blank_matrix <- matrix(blank_matrix, nrow = nrow(restaurants))
    colnames(blank_matrix) <- present
  }

  issue_count <- rowSums(blank_matrix)
  if (all(issue_count == 0)) {
    cli::cli_alert_info(
      "No issues to diagnose - all {nrow(restaurants)} row{?s} complete \\
       on {.val {present}}"
    )
    return(invisible(NULL))
  }

  issues <- apply(blank_matrix, 1, function(r) {
    paste(present[as.logical(r)], collapse = ", ")
  })

  diag <- restaurants
  diag$issues <- issues
  diag$issue_count <- as.integer(issue_count)
  diag <- diag[diag$issue_count > 0, , drop = FALSE]
  diag <- diag[order(-diag$issue_count), , drop = FALSE]

  utils::write.csv(diag, output_path, row.names = FALSE)
  cli::cli_alert_success(
    "Diagnostics written to {.file {output_path}} \\
     ({nrow(diag)} flagged row{?s}, worst-first)"
  )
  invisible(output_path)
}


#' Test whether a vector's entries are "missing data"
#'
#' Treats NA, empty string, and pure-whitespace strings as blank. For
#' numeric columns, only NA counts as blank (an explicit `0` for
#' `price_range` is a real value, not a gap). For factor columns we
#' coerce to character first.
#' @noRd
is_blank_field <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    return(is.na(x) | !nzchar(trimws(x)))
  }
  is.na(x)
}
