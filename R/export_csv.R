#' Export restaurants to CSV
#'
#' Writes all restaurant data to a CSV file, including rows without
#' coordinates.
#'
#' @param restaurants A tibble as returned by [scrape_broadsheet()] or
#'   [geocode_restaurants()].
#' @param output_path Character. File path for the CSV output.
#'
#' @return The output path (invisibly).
#' @export
export_csv <- function(restaurants, output_path = "hotlist.csv") {
  tryCatch(
    {
      utils::write.csv(restaurants, output_path, row.names = FALSE)
      cli::cli_alert_success("CSV written to {.file {output_path}} ({nrow(restaurants)} venues)")
    },
    error = function(e) {
      cli::cli_abort(c(
        "Failed to write CSV to {.file {output_path}}.",
        "i" = conditionMessage(e)
      ))
    }
  )
  invisible(output_path)
}
