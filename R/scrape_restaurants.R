#' Scrape restaurants from a specified source
#'
#' Unified dispatcher that calls the appropriate scraper for the given source.
#' All scrapers return a tibble with the same schema.
#'
#' @param city Character. City to scrape. Default `"sydney"`.
#' @param source Character. Source to scrape from. One of `"broadsheet"`,
#'   `"gourmet_traveller"`, `"timeout"`, `"urban_list"`, `"agfg"`,
#'   `"good_food_guide"`, `"gfg_awards"`, `"concrete_playground"`,
#'   `"infatuation"`, `"eater"`, `"7x7"`, `"cn_traveler"`,
#'   `"james_beard"`, `"michelin"`, `"sprudge"`, `"thrillist"`,
#'   `"honolulu_magazine"`, `"hale_aina"`, `"resy"`, `"bonappetit"`,
#'   `"worlds50best"`. Default `"broadsheet"`.
#' @param use_chromote Logical. Force headless Chrome rendering where applicable.
#'   Default `FALSE`.
#' @param use_cache Logical. If `TRUE`, cache HTTP responses locally to avoid
#'   re-fetching during development. Cached responses expire after 24 hours.
#'   Default `FALSE`.
#' @param use_parsed_cache Logical. If `TRUE` (and `use_cache` is also
#'   `TRUE`), wrap the scrape in `cached_scrape()` so the parsed tibble
#'   itself is cached in `cache/parsed/{source}_{city}.rds`. Invalidates
#'   automatically when any underlying HTML cache file changes. Skipped
#'   when `use_cache = FALSE` (the user explicitly wants fresh data).
#'   Default `TRUE`.
#'
#' @return A tibble with columns: name, suburb, address, cuisine, category,
#'   description, price_range, rating, rating_scale, latitude, longitude, url.
#' @export
scrape_restaurants <- function(city = "sydney",
                               source = "broadsheet",
                               use_chromote = FALSE,
                               use_cache = FALSE,
                               use_parsed_cache = TRUE) {
  source <- match.arg(source, valid_sources())
  city <- validate_city_source(city, source)

  do_scrape <- function() scrape_dispatch(source, city, use_chromote, use_cache)

  if (isTRUE(use_parsed_cache) && isTRUE(use_cache)) {
    cached_scrape(key = paste0(source, "_", city), do_scrape())
  } else {
    do_scrape()
  }
}


#' Internal dispatch from source name -> scrape function call
#'
#' Split out of scrape_restaurants() so cached_scrape() can wrap a single
#' expression cleanly without nesting two switch() statements.
#' @noRd
scrape_dispatch <- function(source, city, use_chromote, use_cache) {
  # gfg_awards and james_beard read from in-memory hand-curated lists
  # rather than HTTP, so use_cache is intentionally not passed - there's
  # nothing to cache.
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
    eater             = scrape_eater(city, use_cache = use_cache),
    `7x7`             = scrape_7x7(city, use_cache = use_cache),
    cn_traveler       = scrape_cn_traveler(city, use_cache = use_cache),
    james_beard       = scrape_james_beard(city),
    michelin          = scrape_michelin(city, use_cache = use_cache),
    sprudge           = scrape_sprudge(city, use_cache = use_cache),
    thrillist         = scrape_thrillist(city, use_cache = use_cache),
    honolulu_magazine = scrape_honolulu_magazine(city, use_cache = use_cache),
    hale_aina         = scrape_hale_aina(city, use_cache = use_cache),
    resy              = scrape_resy(city, use_cache = use_cache),
    bonappetit        = scrape_bonappetit(city, use_cache = use_cache),
    worlds50best      = scrape_worlds50best(city, use_cache = use_cache),
    cli::cli_abort("Internal: dispatcher missing case for {.val {source}}")
  )
}

#' Scrape restaurants from all available sources for a city
#'
#' Runs every scraper that supports the given city and combines results.
#' Sources that fail are warned about but don't stop execution.
#'
#' @param city Character. City to scrape. Default `"sydney"`.
#' @param use_cache Logical. Cache HTTP responses. Default `FALSE`.
#' @param use_parsed_cache Logical. Cache parsed tibbles per (source,
#'   city). Auto-invalidates when underlying HTML cache files change.
#'   Defaults to `TRUE`; only effective when `use_cache = TRUE`.
#' @param skip_sources Character vector of source names to exclude
#'   from this run. Useful when a specific scraper is known to hang
#'   or crash on a given city (e.g. Michelin's NY guide aborts the
#'   R process mid-parse) and you want partial-pipeline results from
#'   the other sources. Defaults to `character(0)` (no skips).
#'
#' @return A tibble with all standard columns plus a `source` column
#'   identifying which guide each venue came from.
#' @export
scrape_all_sources <- function(city = "sydney", use_cache = FALSE,
                               use_parsed_cache = TRUE,
                               skip_sources = character()) {
  city <- tolower(city)
  all_sources <- valid_sources()

  # Find which sources support this city, minus any explicitly skipped.
  supported <- purrr::keep(all_sources, function(src) {
    city %in% supported_cities_for_source(src) && !src %in% skip_sources
  })

  if (length(supported) == 0) {
    cli::cli_abort("No sources support city {.val {city}}.")
  }

  cli::cli_h1("Scraping {length(supported)} source{?s} for {city}")
  cli::cli_alert_info("Sources: {.val {supported}}")

  results <- purrr::map(supported, function(src) {
    cli::cli_rule()
    tryCatch({
      tbl <- scrape_restaurants(city = city, source = src,
                                use_cache = use_cache,
                                use_parsed_cache = use_parsed_cache)
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
