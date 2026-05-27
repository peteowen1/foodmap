#' James Beard Awards - JBA-recognised restaurants by city
#'
#' Returns a hand-curated list of restaurants that have been winners
#' or finalists for major James Beard Awards across recent years. The
#' JBA site itself uses client-side rendering that defeats static
#' HTML scraping (Next.js BAILOUT_TO_CLIENT_SIDE), so this source
#' mirrors the SMH GFG awards pattern: an embedded tribble that's
#' hand-updated annually. The data is small, public, and
#' authoritative.
#'
#' Cities and the JBA regional category that covers them:
#' - `"san-francisco"` - Best Chef: California / West
#' - `"honolulu"` - Best Chef: Northwest & Pacific + America's Classics
#'
#' @param city Character. One of `"san-francisco"`, `"honolulu"`.
#'   Default `"san-francisco"`.
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
  if (city == "honolulu")    return(jba_records_honolulu())
  if (city == "new-york")    return(jba_records_new_york())
  if (city == "los-angeles") return(jba_records_los_angeles())
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


#' Hand-curated JBA New York City recognition list
#'
#' Best Chef: New York State is the regional JBA category for NYC.
#' America's Classics has named several NYC institutions over the
#' years. Best New Restaurant and Outstanding Restaurant have both
#' had heavy NYC representation across recent decades; the list
#' below is intentionally conservative - winners and recent (2018+)
#' finalists only.
#' @noRd
jba_records_new_york <- function() {
  tibble::tribble(
    ~name,                   ~suburb,         ~category,                          ~year, ~award,    ~cuisine,
    # America's Classics - NYC institutions
    "Russ & Daughters",      "Lower East Side", "America's Classics",            2003L, "Winner",   "Jewish Deli",
    "Katz's Delicatessen",   "Lower East Side", "America's Classics",            2024L, "Winner",   "Jewish Deli",
    "Peter Luger",           "Williamsburg",    "America's Classics",            2002L, "Winner",   "Steakhouse",
    "Sylvia's",              "Harlem",          "America's Classics",            2008L, "Winner",   "Soul Food",
    # Outstanding Restaurant (recent winners and finalists)
    "Eleven Madison Park",   "Flatiron",        "Outstanding Restaurant",        2017L, "Winner",   "Contemporary American",
    "Le Bernardin",          "Midtown West",    "Outstanding Restaurant",        2015L, "Winner",   "Seafood",
    "Daniel",                "Upper East Side", "Outstanding Restaurant",        2010L, "Winner",   "French",
    "Per Se",                "Columbus Circle", "Outstanding Restaurant",        2011L, "Winner",   "Contemporary American",
    "Gramercy Tavern",       "Flatiron",        "Outstanding Restaurant",        2008L, "Winner",   "Contemporary American",
    "Atomix",                "Murray Hill",     "Outstanding Restaurant",        2024L, "Finalist", "Korean",
    "Estela",                "NoLita",          "Outstanding Restaurant",        2019L, "Finalist", "Contemporary American",
    "Marea",                 "Columbus Circle", "Outstanding Restaurant",        2018L, "Finalist", "Italian",
    "Cosme",                 "Flatiron",        "Outstanding Restaurant",        2022L, "Finalist", "Mexican",
    # Best New Restaurant
    "Cote",                  "Flatiron",        "Best New Restaurant",           2018L, "Finalist", "Korean",
    "Misi",                  "Williamsburg",    "Best New Restaurant",           2020L, "Finalist", "Italian",
    "Dirt Candy",            "Lower East Side", "Best New Restaurant",           2016L, "Finalist", "Vegetarian",
    # Best Chef: New York State (recent)
    "Llama Inn",             "Williamsburg",    "Best Chef: New York State",     2023L, "Finalist", "Peruvian",
    "Atoboy",                "NoMad",           "Best Chef: New York State",     2019L, "Finalist", "Korean",
    "Semma",                 "West Village",    "Best Chef: New York State",     2024L, "Winner",   "Indian",
    "Tatiana",               "Lincoln Square",  "Best Chef: New York State",     2024L, "Finalist", "Afro-Caribbean"
  )
}


#' Hand-curated JBA Los Angeles recognition list
#'
#' LA shares the Best Chef: California category with SF/Bay Area; the
#' Outstanding Restaurant and Best New Restaurant categories are
#' national. List is intentionally conservative - winners and recent
#' (2018+) finalists only.
#' @noRd
jba_records_los_angeles <- function() {
  tibble::tribble(
    ~name,                   ~suburb,           ~category,                       ~year, ~award,    ~cuisine,
    # Outstanding Restaurant
    "n/naka",                "Palms",           "Outstanding Restaurant",        2024L, "Finalist", "Japanese",
    "Republique",            "Hancock Park",    "Outstanding Restaurant",        2018L, "Finalist", "French",
    # Best Chef: California (LA-side winners and finalists)
    "Bavel",                 "Arts District",   "Best Chef: California",         2022L, "Finalist", "Middle Eastern",
    "Bestia",                "Arts District",   "Best Chef: California",         2018L, "Finalist", "Italian",
    "Holbox",                "South LA",        "Best Chef: California",         2024L, "Winner",   "Seafood",
    "Damian",                "Arts District",   "Best Chef: California",         2023L, "Finalist", "Mexican",
    "Mh Zh",                 "Silver Lake",     "Best Chef: California",         2022L, "Finalist", "Israeli",
    "Pijja Palace",          "Silver Lake",     "Best Chef: California",         2024L, "Finalist", "Indian",
    "Anajak Thai",           "Sherman Oaks",    "Best Chef: California",         2023L, "Winner",   "Thai",
    # Best New Restaurant
    "Mes Amis",              "Hancock Park",    "Best New Restaurant",           2024L, "Finalist", "French",
    "Yangban",               "Arts District",   "Best New Restaurant",           2023L, "Finalist", "Korean",
    # America's Classics - LA institutions
    "Philippe the Original", "Chinatown",       "America's Classics",            2024L, "Winner",   "American",
    "Langer's Delicatessen", "Westlake",        "America's Classics",            2001L, "Winner",   "Jewish Deli"
  )
}


#' Hand-curated JBA Honolulu / Oʻahu recognition list
#'
#' Best Chef: Northwest & Pacific is the regional JBA category that
#' includes Hawaiʻi; America's Classics is the lifetime-recognition
#' category that has named several long-running local institutions.
#' Both are public, well-documented and stable enough for a static
#' embed.
#'
#' Kept deliberately conservative - only entries that have been
#' widely reported in multiple sources. Outer-island chefs and
#' venues (Maui, Big Island, Kauaʻi) are omitted because they fall
#' outside the Honolulu city bbox and the geocoder would discard
#' them anyway.
#' @noRd
jba_records_honolulu <- function() {
  tibble::tribble(
    ~name,                ~suburb,         ~category,                          ~year, ~award,    ~cuisine,
    # America's Classics - JBA's lifetime-recognition category for
    # long-running local institutions
    "Helena's Hawaiian Food", "Kalihi",     "America's Classics",               2000L, "Winner",   "Hawaiian",
    # Best Chef: Northwest & Pacific (covers AK/HI/OR/WA)
    "Fete",                "Chinatown",     "Best Chef: Northwest & Pacific",   2022L, "Winner",   "Contemporary American",
    "Senia",               "Chinatown",     "Best Chef: Northwest & Pacific",   2018L, "Finalist", "Contemporary American",
    "The Pig and The Lady","Chinatown",     "Best Chef: Northwest & Pacific",   2019L, "Finalist", "Vietnamese",
    "MW Restaurant",       "Ala Moana",     "Best Chef: Northwest & Pacific",   2019L, "Finalist", "Contemporary American",
    "Koko Head Cafe",      "Kaimuk\u012B",       "Best Chef: Northwest & Pacific",   2017L, "Finalist", "Brunch",
    "Mud Hen Water",       "Kaimuk\u012B",       "Best Chef: Northwest & Pacific",   2017L, "Finalist", "Contemporary American",
    # Best New Restaurant finalists
    "Senia",               "Chinatown",     "Best New Restaurant",              2017L, "Finalist", "Contemporary American",
    # Outstanding Restaurant historical recognitions
    "La Mer",              "Waik\u012Bk\u012B",       "Outstanding Restaurant",           2015L, "Finalist", "French",
    "Chef Mavro",          "Ala Moana",     "Outstanding Restaurant",           2014L, "Finalist", "Contemporary American"
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
