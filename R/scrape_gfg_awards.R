#' Load SMH Good Food Guide 2026 hatted restaurants
#'
#' Returns the full list of hat-winning restaurants from the SMH Good Food
#' Guide 2026 Awards (Sydney/NSW/ACT edition). Sourced from the SMH
#' awards article (October 2025). Unlike [scrape_good_food_guide()] which
#' fetches individual review pages via chromote (limited by the paywall),
#' this returns the curated annual awards list with hat counts only - useful
#' for high-recall coverage of every hatted venue without per-review scraping.
#'
#' @param city Character. Currently only `"sydney"` (the SMH guide covers
#'   NSW + ACT). Default `"sydney"`.
#'
#' @return A tibble with the standard scraper schema plus a `hats` column
#'   (integer, 1-3). Venues new to the 2026 guide are flagged in the
#'   `description` field.
#' @export
#' @examples
#' \dontrun{
#'   awards <- scrape_gfg_awards("sydney")
#'   dplyr::count(awards, hats)
#' }
scrape_gfg_awards <- function(city = "sydney") {
  city <- validate_city_source(city, "gfg_awards")
  cli::cli_h1("Loading SMH Good Food Guide 2026 awards: {city}")

  raw <- gfg_awards_2026_records()
  out <- gfg_awards_to_tibble(raw)

  n_total <- nrow(out)
  n3 <- sum(out$hats == 3)
  n2 <- sum(out$hats == 2)
  n1 <- sum(out$hats == 1)
  cli::cli_alert_success(
    "{n_total} hatted venue{?s}: {n3} three-hat, {n2} two-hat, {n1} one-hat"
  )
  out
}


#' Raw 2026 awards data
#'
#' Hand-curated from the SMH Good Food Guide 2026 awards page (archive.is/z1iVx,
#' published 2025-10-13). Each row is one venue. `suburb = NA` is intentional
#' for the few venues the article listed without a suburb. `is_new = TRUE`
#' marks restaurants flagged "NEW" (debuting in the 2026 guide).
#'
#' @return Tibble with columns: name, suburb, hats, is_new
#' @noRd
gfg_awards_2026_records <- function() {
  three_hats <- tibble::tribble(
    ~name,                         ~suburb,
    "Oncore by Clare Smyth",       "Barangaroo",
    "Quay",                        "The Rocks",
    "Saint Peter",                 "Paddington",
    "Sixpenny",                    "Stanmore"
  ) |> dplyr::mutate(hats = 3L, is_new = FALSE)

  two_hats <- tibble::tribble(
    ~name,                                       ~suburb,           ~is_new,
    # City and suburbs
    "a'Mare",                                    "Barangaroo",       FALSE,
    "Aalia",                                     "Sydney CBD",       FALSE,
    "Allta",                                     "Surry Hills",      FALSE,
    "Aria",                                      "Sydney CBD",       FALSE,
    "Bathers' Pavilion",                         "Balmoral",         FALSE,
    "Bennelong",                                 "Bennelong Point",  FALSE,
    "Bentley Restaurant + Bar",                  "Sydney CBD",       FALSE,
    "Berowra Waters Inn",                        "Berowra Waters",   FALSE,
    "Cafe Paci",                                 "Newtown",          FALSE,
    "Eleven Barrack",                            "Sydney CBD",       TRUE,
    "Ester",                                     "Chippendale",      FALSE,
    "Firedoor",                                  "Surry Hills",      FALSE,
    "Garaku",                                    "Sydney CBD",       TRUE,
    "Icebergs Dining Room and Bar",              "Bondi Beach",      FALSE,
    "King Clarence",                             "Sydney CBD",       FALSE,
    "LuMi Dining",                               "Pyrmont",          FALSE,
    "Margaret",                                  "Double Bay",       FALSE,
    "Ormeggio at The Spit",                      "Mosman",           FALSE,
    "Pilu at Freshwater",                        "Freshwater",       FALSE,
    "Porteno",                                   "Surry Hills",      FALSE,
    "Rockpool Bar & Grill",                      "Sydney CBD",       FALSE,
    "Shell House Dining Room & Terrace",         "Sydney CBD",       FALSE,
    "Spice Temple",                              "Sydney CBD",       FALSE,
    "Sushi Oe",                                  "Cammeray",         FALSE,
    "The Grill at The International",            "Sydney CBD",       TRUE,
    "Ursula's",                                  "Paddington",       FALSE,
    "Yellow",                                    "Potts Point",      FALSE,
    "Yoshii's Omakase",                          "Barangaroo",       FALSE,
    # Regional and ACT
    "Bistro Livi",                               "Murwillumbah",     FALSE,
    "EXP. Restaurant",                           "Pokolbin",         FALSE,
    "Muse Restaurant",                           "Pokolbin",         FALSE,
    "Pilot",                                     "Ainslie ACT",      FALSE,
    "Pipit",                                     "Pottsville",       FALSE,
    "Raes Dining Room",                          "Byron Bay",        FALSE
  ) |> dplyr::mutate(hats = 2L)

  one_hat <- tibble::tribble(
    ~name,                                            ~suburb,                 ~is_new,
    # City and suburbs
    "10 William St",                                  "Paddington",             FALSE,
    "20 Chapel",                                      "Marrickville",           FALSE,
    "Abhi's Indian Restaurant",                       "North Strathfield",      FALSE,
    "Amuro",                                          "Darlinghurst",           FALSE,
    "Ante",                                           "Newtown",                FALSE,
    "Attenzione!",                                    "Redfern",                FALSE,
    "Baba's Place",                                   "Marrickville",           FALSE,
    "Bar Copains",                                    "Surry Hills",            FALSE,
    "Bastardo",                                       "Surry Hills",            FALSE,
    "Bessie's",                                       "Surry Hills",            FALSE,
    "Bistro George",                                  "Sydney CBD",             FALSE,
    "Canvas",                                         "Sydney CBD",             TRUE,
    "Catalina",                                       "Rose Bay",               FALSE,
    "The Charles Brasserie & Bar",                    "Sydney CBD",             FALSE,
    "Cho Cho San",                                    "Potts Point",            FALSE,
    "Cibaria",                                        "Manly",                  TRUE,
    "Civico 47",                                      "Paddington",             FALSE,
    "Clam Bar",                                       "Sydney CBD",             FALSE,
    "Continental Deli",                               "Newtown",                FALSE,
    "Corner 75",                                      "Randwick",               TRUE,
    "Cricca",                                         "Windsor",                FALSE,
    "The Cut",                                        "The Rocks",              TRUE,
    "Da Orazio",                                      "Bondi Beach",            FALSE,
    "The Dining Room by James Viles",                 "The Rocks",              FALSE,
    "The Dry Dock",                                   "Balmain",                FALSE,
    "Firepop",                                        "Enmore",                 FALSE,
    "Fontana",                                        "Redfern",                FALSE,
    "The Gidley",                                     "Sydney CBD",             FALSE,
    "Gildas",                                         "Surry Hills",            FALSE,
    "Gran Torino",                                    "Double Bay",             TRUE,
    "Grandfathers",                                   "Sydney CBD",             TRUE,
    "Haco",                                           "Sydney CBD",             FALSE,
    "Ho Jiak Town Hall",                              "Sydney CBD",             FALSE,
    # The 5 venues below appeared in the SMH article without a suburb
    # listed. Setting suburb to "Sydney" gives the geocoder a city
    # hint - without it, ambiguous names like "Yan" can resolve to
    # the wrong city.
    "Ibushi",                                         "Sydney",                 TRUE,
    "Infinity by Mark Best",                          "Sydney CBD",             TRUE,
    "Ito",                                            "Surry Hills",            FALSE,
    "Jane",                                           "Surry Hills",            FALSE,
    "Kiln",                                           "Sydney CBD",             FALSE,
    "Kindred",                                        "Darlington",             FALSE,
    "Kisuke",                                         "Potts Point",            FALSE,
    "Kuon Omakase",                                   "Surry Hills",            FALSE,
    "Lottie",                                         "Redfern",                TRUE,
    "Loulou Bistro Martin Place",                     "Sydney CBD",             TRUE,
    "Loulou Bistro",                                  "Milsons Point",          FALSE,
    "Maiz",                                           "Newtown",                FALSE,
    "Maydanoz",                                       "Sydney CBD",             FALSE,
    "Mister Grotto",                                  "Newtown",                TRUE,
    "Neptune's Grotto",                               "Sydney CBD",             TRUE,
    "Nour",                                           "Surry Hills",            FALSE,
    "Olympus Dining",                                 "Redfern",                TRUE,
    "Omakase by Prefecture 48",                       "Sydney",                 TRUE,
    "Osteria di Russo & Russo",                       "Enmore",                 FALSE,
    "Osteria Mucca",                                  "Newtown",                TRUE,
    "Otto Ristorante",                                "Woolloomooloo",          FALSE,
    "Palazzo Salato",                                 "Sydney CBD",             FALSE,
    "Pellegrino 2000",                                "Surry Hills",            FALSE,
    "Poly",                                           "Surry Hills",            FALSE,
    "Porcine",                                        "Paddington",             FALSE,
    "Porkfat",                                        "Haymarket",              FALSE,
    "Postino Osteria",                                "Summer Hill",            TRUE,
    "R by Raita Noda",                                "Redfern",                TRUE,
    "RAFI",                                           "North Sydney",           FALSE,
    "Restaurant Ka",                                  "Darlinghurst",           FALSE,
    "Royal Palace Seafood Restaurant",                "Haymarket",              FALSE,
    "Sala",                                           "Pyrmont",                FALSE,
    "Sean's",                                         "North Bondi",            FALSE,
    "Sinclair's",                                     "Penrith",                FALSE,
    "Sokyo",                                          "Pyrmont",                FALSE,
    "Soul Dining",                                    "Sydney CBD",             FALSE,
    "Sydney Common",                                  "Sydney CBD",             FALSE,
    "Taste of Shunde",                                "Hurstville",             FALSE,
    "Tilda",                                          "Sydney CBD",             TRUE,
    "Viand",                                          "Woolloomooloo",          FALSE,
    "Vin-Cenzo's",                                    "Darlinghurst",           TRUE,
    "Vineria Luisa",                                  "Enmore",                 TRUE,
    "The White Horse",                                "Sydney",                 FALSE,
    "Woodcut",                                        "Barangaroo",             FALSE,
    "Yan",                                            "Sydney",                 FALSE,
    "Yeodongsik",                                     "Sydney",                 FALSE,
    # Regional and ACT
    "Ates",                                           "Blackheath",             FALSE,
    "Bangalay Dining",                                "Shoalhaven Heads",       FALSE,
    "Bar Heather",                                    "Byron Bay",              FALSE,
    "Bar Rochford",                                   "Canberra",               FALSE,
    "Beach Byron Bay",                                "Byron Bay",              FALSE,
    "Bistro Molines",                                 "Mount View",             FALSE,
    "Bistro Penny",                                   "Newcastle",              TRUE,
    "Flotilla",                                       "Wickham",                FALSE,
    "Frida's Field",                                  "Nashua",                 FALSE,
    "Hey Rosey",                                      "Orange",                 FALSE,
    "Humbug",                                         "Newcastle",              FALSE,
    "Lunetta",                                        "Red Hill ACT",           TRUE,
    "Megalong Restaurant",                            "Megalong Valley",        FALSE,
    "The Milton Hotel",                               "Milton",                 FALSE,
    "Mu Omakase",                                     "Canberra",               FALSE,
    "Norma",                                          "Albury",                 FALSE,
    "Onzieme",                                        "Kingston ACT",           FALSE,
    "Paper Daisy",                                    "Cabarita Beach",         FALSE,
    "Paranormal Wines",                               "Campbell ACT",           FALSE,
    "Paste Australia",                                "Mittagong",              FALSE,
    "Printhie Dining",                                "Nashdale",               FALSE,
    "Rebel Rebel",                                    "Acton ACT",              FALSE,
    "Restaurant Santino",                             "Wollongong",             FALSE,
    "Roco Ramen",                                     "Brunswick Heads",        FALSE,
    "South on Albany",                                "Berry",                  FALSE,
    "Stonefruit",                                     "Tenterfield",            FALSE,
    "Such and Such",                                  "Canberra",               FALSE,
    "You Beauty",                                     "Bangalow",               FALSE,
    "The Zin House",                                  "Eurunderee",             FALSE
  ) |> dplyr::mutate(hats = 1L)

  dplyr::bind_rows(three_hats, two_hats, one_hat)
}


#' Convert raw award records to the package's standard scraper tibble
#'
#' Maps the parsed (name, suburb, hats, is_new) records onto the package
#' schema used by every other scraper, so this source can flow through
#' `harmonize_sources()`, `deduplicate_restaurants()`, `geocode_restaurants()`
#' and `export_kml()` unchanged.
#'
#' TODO (user contribution): Decide how the hat tier and the "NEW for 2026"
#' flag map onto the package schema. See the prompt below for the choices
#' that need making.
#'
#' @param records Tibble from `gfg_awards_2026_records()` with columns
#'   `name`, `suburb`, `hats` (int 1-3), `is_new` (logical).
#' @return A tibble matching the package's standard scraper schema.
#' @noRd
gfg_awards_to_tibble <- function(records) {
  awards_url <- "https://www.smh.com.au/goodfood/sydney-eating-out/sydney-good-food-guide-2026-hatted-restaurants-20251008-p5n07j.html"

  dplyr::transmute(
    records,
    name         = .data$name,
    suburb       = .data$suburb,
    address      = NA_character_,
    cuisine      = NA_character_,
    category     = "Restaurant",
    description  = paste0(
      "SMH Good Food Guide 2026 - ", .data$hats, " hat",
      ifelse(.data$hats > 1, "s", ""),
      ifelse(.data$is_new, " (NEW for 2026)", "")
    ),
    price_range  = NA_integer_,
    rating       = as.double(.data$hats),
    rating_scale = "gfg_hats_3",
    hats         = .data$hats,
    latitude     = NA_real_,
    longitude    = NA_real_,
    url          = awards_url
  )
}
