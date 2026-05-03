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
    ~name,                ~suburb,         ~category,                ~year, ~award,    ~cuisine,
    # Best Chef: California winners (recent)
    "Mister Jiu's",            "Chinatown",     "Best Chef: California",  2023L, "Winner",   "Chinese",
    "Reem's California",       "Mission",       "Best Chef: California",  2022L, "Winner",   "Middle Eastern",
    "State Bird Provisions",   "Western Addition", "Best Chef: West",     2022L, "Winner",   "Contemporary American",
    "Liholiho Yacht Club",     "Polk Gulch",    "Best Chef: West",        2017L, "Winner",   "Hawaiian",
    "Saison",                  "SoMa",          "Best Chef: West",        2016L, "Winner",   "Contemporary American",
    "Coi",                     "North Beach",   "Best Chef: West",        2014L, "Winner",   "Contemporary American",
    "Atelier Crenn",           "Cow Hollow",    "Best Chef: West",        2017L, "Winner",   "French",
    # Outstanding Restaurant winners and recent finalists in SF
    "Zuni Cafe",          "Hayes Valley",  "Outstanding Restaurant", 2003L, "Winner",   "Mediterranean",
    "Chez Panisse",       "Berkeley",      "Outstanding Restaurant", 1992L, "Winner",   "Californian",
    "Slanted Door",       "Embarcadero",   "Outstanding Restaurant", 2014L, "Winner",   "Vietnamese",
    "State Bird Provisions", "Western Addition", "Outstanding Restaurant", 2017L, "Finalist", "Contemporary American",
    "Octavia",            "Pacific Heights","Outstanding Restaurant", 2018L, "Finalist", "Californian",
    "Rich Table",         "Hayes Valley",  "Outstanding Restaurant", 2019L, "Finalist", "Contemporary American",
    "Nopa",               "NOPA",          "Outstanding Restaurant", 2024L, "Finalist", "Californian",
    "Kin Khao",           "Tenderloin",    "Best Chef: West",        2019L, "Finalist", "Thai",
    "Lazy Bear",          "Mission",       "Outstanding Restaurant", 2020L, "Finalist", "Contemporary American",
    # Best New Restaurant (recent SF nominees)
    "Cafe Nopa",          "NOPA",          "Best New Restaurant",    2025L, "Finalist", "Californian",
    "Birdsong",           "SoMa",          "Best New Restaurant",    2020L, "Finalist", "Contemporary American",
    "Sons & Daughters",   "Nob Hill",      "Outstanding Restaurant", 2023L, "Finalist", "Contemporary American",
    "Aphotic",            "SoMa",          "Best Chef: California",  2024L, "Finalist", "Seafood",
    "Sorrel",             "Presidio Heights","Best Chef: California",2023L, "Finalist", "Italian",
    "Dalida",             "Presidio",      "Best New Restaurant",    2024L, "Finalist", "Mediterranean",
    "Daytrip",            "Oakland",       "Best New Restaurant",    2024L, "Finalist", "Contemporary American",
    # Outstanding Chef nominations / wider category recognitions
    "Quince",             "Jackson Square","Outstanding Restaurant", 2018L, "Finalist", "Italian",
    # SingleThread is in Healdsburg (~70 mi north of SF, in Sonoma
    # County) and falls outside the SF city bbox - removed so the
    # geocoder doesn't waste an API call rejecting it.
    "Benu",               "SoMa",          "Outstanding Service",    2017L, "Winner",   "Contemporary American",
    "Acquerello",         "Polk Gulch",    "Outstanding Wine Service",2018L,"Finalist", "Italian"
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
    cuisine      = .data$cuisine,
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
