#' Create a food map from restaurant guide sources
#'
#' One-call pipeline: scrapes a restaurant guide, geocodes venues
#' missing coordinates via Google Places API, and exports KML + CSV files
#' ready for import into Google My Maps.
#'
#' @param city Character. City to scrape. Default `"sydney"`.
#' @param source Character. Source to scrape from. One of `"broadsheet"`,
#'   `"gourmet_traveller"`, `"timeout"`, `"urban_list"`, `"agfg"`,
#'   `"good_food_guide"`. Default `"broadsheet"`.
#' @param output_dir Character. Directory for output files. Created if needed.
#'   Defaults to the current working directory.
#' @param api_key Character. Google Places API key. Defaults to the
#'   `GOOGLE_PLACES_API_KEY` environment variable.
#' @param use_chromote Logical. Force headless Chrome rendering.
#'   Default `FALSE` (auto-falls back if needed).
#' @param use_cache Logical. Cache HTTP responses locally to avoid re-fetching.
#'   Default `FALSE`.
#'
#' @return A tibble of geocoded restaurants (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' Sys.setenv(GOOGLE_PLACES_API_KEY = "your-key")
#' create_food_map("sydney", output_dir = "~/maps")
#' create_food_map("sydney", source = "timeout", output_dir = "~/maps")
#' }
create_food_map <- function(city = "sydney",
                            source = "broadsheet",
                            output_dir = ".",
                            api_key = NULL,
                            use_chromote = FALSE,
                            use_cache = FALSE) {

  source <- match.arg(source, valid_sources())
  city <- validate_city_source(city, source)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cli::cli_alert_info("Created output directory: {.path {output_dir}}")
  }

  # 1. Scrape
  restaurants <- scrape_restaurants(
    city = city, source = source, use_chromote = use_chromote,
    use_cache = use_cache
  )

  if (nrow(restaurants) == 0) {
    cli::cli_abort("Scraping returned 0 venues for {.val {source}} in {.val {city}}.")
  }

  restaurants$source <- source

  # 2. Geocode (only rows missing coordinates)
  restaurants <- geocode_restaurants(restaurants, api_key = api_key)

  # 3. Export
  file_label <- glue::glue("{city}_{source}")
  kml_path <- file.path(output_dir, glue::glue("{file_label}.kml"))
  csv_path <- file.path(output_dir, glue::glue("{file_label}.csv"))

  export_kml(restaurants, kml_path)
  export_csv(restaurants, csv_path)

  cli::cli_h2("Done!")
  cli::cli_alert_info("Import {.file {kml_path}} at {.url https://mymaps.google.com}")

  invisible(restaurants)
}
