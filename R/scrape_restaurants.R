#' Scrape restaurants from a specified source
#'
#' Unified dispatcher that calls the appropriate scraper for the given source.
#' All scrapers return a tibble with the same schema.
#'
#' @param city Character. City to scrape. Default `"sydney"`.
#' @param source Character. Source to scrape from. One of `"broadsheet"`,
#'   `"gourmet_traveller"`, `"timeout"`, `"urban_list"`, `"agfg"`,
#'   `"good_food_guide"`, `"gfg_awards"`. Default `"broadsheet"`.
#' @param use_chromote Logical. Force headless Chrome rendering where applicable.
#'   Default `FALSE`.
#' @param use_cache Logical. If `TRUE`, cache HTTP responses locally to avoid
#'   re-fetching during development. Cached responses expire after 24 hours.
#'   Default `FALSE`.
#'
#' @return A tibble with columns: name, suburb, address, cuisine, category,
#'   description, price_range, rating, rating_scale, latitude, longitude, url.
#' @export
scrape_restaurants <- function(city = "sydney",
                               source = "broadsheet",
                               use_chromote = FALSE,
                               use_cache = FALSE) {
  source <- match.arg(source, valid_sources())
  city <- validate_city_source(city, source)

  switch(source,
    broadsheet        = scrape_broadsheet(city, use_chromote = use_chromote,
                                          use_cache = use_cache),
    gourmet_traveller = scrape_gourmet_traveller(city, use_cache = use_cache),
    timeout           = scrape_timeout(city, use_cache = use_cache),
    urban_list        = scrape_urban_list(city, use_cache = use_cache),
    agfg              = scrape_agfg(city, use_cache = use_cache),
    good_food_guide   = scrape_good_food_guide(city, use_cache = use_cache),
    gfg_awards        = scrape_gfg_awards(city),
    concrete_playground = scrape_concrete_playground(city, use_cache = use_cache),
    infatuation       = scrape_infatuation(city, use_cache = use_cache),
    eater             = scrape_eater(city, use_cache = use_cache)
  )
}

#' Scrape restaurants from all available sources for a city
#'
#' Runs every scraper that supports the given city and combines results.
#' Sources that fail are warned about but don't stop execution.
#'
#' @param city Character. City to scrape. Default `"sydney"`.
#' @param use_cache Logical. Cache HTTP responses. Default `FALSE`.
#'
#' @return A tibble with all standard columns plus a `source` column
#'   identifying which guide each venue came from.
#' @export
scrape_all_sources <- function(city = "sydney", use_cache = FALSE) {
  city <- tolower(city)
  all_sources <- valid_sources()

  # Find which sources support this city
  supported <- purrr::keep(all_sources, function(src) {
    city %in% supported_cities_for_source(src)
  })

  if (length(supported) == 0) {
    cli::cli_abort("No sources support city {.val {city}}.")
  }

  cli::cli_h1("Scraping {length(supported)} source{?s} for {city}")
  cli::cli_alert_info("Sources: {.val {supported}}")

  results <- purrr::map(supported, function(src) {
    cli::cli_rule()
    tryCatch({
      tbl <- scrape_restaurants(city = city, source = src, use_cache = use_cache)
      tbl$source <- src
      tbl
    }, error = function(e) {
      cli::cli_warn("Source {.val {src}} failed: {conditionMessage(e)}")
      NULL
    })
  }) |>
    purrr::compact()

  if (length(results) == 0) {
    cli::cli_abort("All sources failed for {.val {city}}.")
  }

  combined <- dplyr::bind_rows(results)
  cli::cli_h2("Combined results")
  cli::cli_alert_success("{nrow(combined)} venue{?s} from {length(results)} source{?s}")
  combined
}
