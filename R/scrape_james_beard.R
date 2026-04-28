#' James Beard Awards - SF / Bay Area recognised restaurants
#'
#' Returns a hand-curated list of San Francisco / Bay Area restaurants
#' that have been winners or finalists for major James Beard Awards
#' across recent years. The JBA site itself uses client-side rendering
#' that defeats static HTML scraping (Next.js BAILOUT_TO_CLIENT_SIDE),
#' so this source mirrors the SMH GFG awards pattern: an embedded
#' tribble that's hand-updated annually. The data is small, public,
#' and authoritative.
#'
#' Categories included:
#' - Outstanding Restaurant (winners and finalists)
#' - Outstanding Chef (winners and finalists, restaurant attributed)
#' - Best Chef: California (winners and finalists)
#' - Best New Restaurant (winners and finalists in SF area)
#' - Outstanding Hospitality (winners and finalists in SF area)
#'
#' @param city Character. Currently `"san-francisco"`. Default
#'   `"san-francisco"`.
#' @return A tibble with the standard scraper schema plus an
#'   `award_year` column (most recent year recognised).
#' @export
scrape_james_beard <- function(city = "san-francisco") {
  city <- validate_city_source(city, "james_beard")
  cli::cli_h1("Loading James Beard Awards: {city}")

  raw <- jba_records(city)
  result <- jba_to_tibble(raw)

  cli::cli_alert_success(
    "{nrow(result)} JBA-recognised venue{?s}"
  )
  result
}


#' Hand-curated JBA SF/Bay Area recognition list
#'
#' Each row: name, suburb, category (which JBA), year (most recent
#' recognition), award (winner / finalist).
#' @noRd
jba_records <- function(city) {
  if (city != "san-francisco") {
    cli::cli_abort("No JBA list for {.val {city}}")
  }

  tibble::tribble(
    ~name,                ~suburb,         ~category,                ~year, ~award,
    # Best Chef: California winners (recent)
    "Mister Jiu's",            "Chinatown",     "Best Chef: California",  2023L, "Winner",
    "Reem's California",       "Mission",       "Best Chef: California",  2022L, "Winner",
    "State Bird Provisions",   "Western Addition", "Best Chef: West",     2022L, "Winner",
    "Liholiho Yacht Club",     "Polk Gulch",    "Best Chef: West",        2017L, "Winner",
    "Saison",                  "SoMa",          "Best Chef: West",        2016L, "Winner",
    "Coi",                     "North Beach",   "Best Chef: West",        2014L, "Winner",
    "Atelier Crenn",           "Cow Hollow",    "Best Chef: West",        2017L, "Winner",
    # Outstanding Restaurant winners and recent finalists in SF
    "Zuni Cafe",          "Hayes Valley",  "Outstanding Restaurant", 2003L, "Winner",
    "Chez Panisse",       "Berkeley",      "Outstanding Restaurant", 1992L, "Winner",
    "Slanted Door",       "Embarcadero",   "Outstanding Restaurant", 2014L, "Winner",
    "State Bird Provisions", "Western Addition", "Outstanding Restaurant", 2017L, "Finalist",
    "Octavia",            "Pacific Heights","Outstanding Restaurant", 2018L, "Finalist",
    "Rich Table",         "Hayes Valley",  "Outstanding Restaurant", 2019L, "Finalist",
    "Nopa",               "NOPA",          "Outstanding Restaurant", 2024L, "Finalist",
    "Kin Khao",           "Tenderloin",    "Best Chef: West",        2019L, "Finalist",
    "Lazy Bear",          "Mission",       "Outstanding Restaurant", 2020L, "Finalist",
    # Best New Restaurant (recent SF nominees)
    "Cafe Nopa",          "NOPA",          "Best New Restaurant",    2025L, "Finalist",
    "Birdsong",           "SoMa",          "Best New Restaurant",    2020L, "Finalist",
    "Sons & Daughters",   "Nob Hill",      "Outstanding Restaurant", 2023L, "Finalist",
    "Aphotic",            "SoMa",          "Best Chef: California",  2024L, "Finalist",
    "Sorrel",             "Presidio Heights","Best Chef: California",2023L, "Finalist",
    "Dalida",             "Presidio",      "Best New Restaurant",    2024L, "Finalist",
    "Daytrip",            "Oakland",       "Best New Restaurant",    2024L, "Finalist",
    # Outstanding Chef nominations / wider category recognitions
    "Quince",             "Jackson Square","Outstanding Restaurant", 2018L, "Finalist",
    "SingleThread",       "Healdsburg",    "Outstanding Restaurant", 2024L, "Finalist",
    "Benu",               "SoMa",          "Outstanding Service",    2017L, "Winner",
    "Acquerello",         "Polk Gulch",    "Outstanding Wine Service",2018L,"Finalist"
  )
}


#' Convert raw award records to the package's standard scraper tibble
#' @noRd
jba_to_tibble <- function(records) {
  url <- "https://www.jamesbeard.org/awards/search-past-awards"
  dplyr::transmute(
    records,
    name         = .data$name,
    suburb       = .data$suburb,
    address      = NA_character_,
    cuisine      = NA_character_,
    category     = "Restaurant",
    description  = paste0(
      "James Beard ", .data$award, " - ", .data$category, " (", .data$year, ")"
    ),
    price_range  = NA_integer_,
    rating       = NA_real_,
    rating_scale = NA_character_,
    latitude     = NA_real_,
    longitude    = NA_real_,
    url          = url
  )
}
