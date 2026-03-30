#' Scrape restaurants from a specified source
#'
#' Unified dispatcher that calls the appropriate scraper for the given source.
#' All scrapers return a tibble with the same schema.
#'
#' @param city Character. City to scrape. Default `"sydney"`.
#' @param source Character. Source to scrape from. One of `"broadsheet"`,
#'   `"gourmet_traveller"`, `"timeout"`, `"urban_list"`, `"agfg"`,
#'   `"good_food_guide"`. Default `"broadsheet"`.
#' @param use_chromote Logical. Force headless Chrome rendering where applicable.
#'   Default `FALSE`.
#'
#' @return A tibble with columns: name, suburb, address, cuisine, category,
#'   description, price_range, latitude, longitude, url.
#' @export
scrape_restaurants <- function(city = "sydney",
                               source = "broadsheet",
                               use_chromote = FALSE) {
  source <- match.arg(source, valid_sources())
  city <- validate_city_source(city, source)

  switch(source,
    broadsheet        = scrape_broadsheet(city, use_chromote = use_chromote),
    gourmet_traveller = scrape_gourmet_traveller(city),
    timeout           = scrape_timeout(city),
    urban_list        = scrape_urban_list(city),
    agfg              = scrape_agfg(city),
    good_food_guide   = scrape_good_food_guide(city)
  )
}
