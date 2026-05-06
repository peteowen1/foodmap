# Cross-scraper cuisine inference helpers ----------------------------------
#
# Several scrapers (CN Traveler, Eater, future ones) need to infer a
# cuisine tag from a paragraph of editorial prose rather than a short
# dish name. The logic is identical across them - look for cuisine
# adjectives ("Italian", "Vietnamese") and venue-type nouns
# ("trattoria", "izakaya", "bistro") - so the matcher lives here as a
# single source of truth instead of being duplicated per scraper.


#' Infer a cuisine tag from a paragraph of editorial prose
#'
#' Editorial reviews routinely tag a venue with an explicit adjective
#' ("Italian trattoria", "Vietnamese pho shop") or a venue-type noun
#' ("izakaya", "taqueria", "bistro"). Those words are far more
#' reliable than dish-name keywords on prose - "the burger here is
#' good" doesn't make a place a burger joint - so the matcher returns
#' as soon as one of those signals fires.
#'
#' Returns `NA_character_` when nothing matches. Callers that want a
#' dish-keyword fallback can call `dish_to_cuisine()` (the 7x7
#' helper) explicitly afterwards, accepting the false-positive risk.
#'
#' @param description Character. The prose to scan.
#' @return Character cuisine tag or `NA_character_`.
#' @noRd
prose_to_cuisine <- function(description) {
  if (is.na(description) || !nzchar(description)) return(NA_character_)
  text <- tolower(description)
  text <- tryCatch(
    stringi::stri_trans_general(text, "Latin-ASCII"),
    error = function(e) text
  )
  for (cuisine in names(.prose_cuisine_patterns)) {
    if (grepl(.prose_cuisine_patterns[[cuisine]], text, perl = TRUE)) {
      return(cuisine)
    }
  }
  NA_character_
}

# Adjective / venue-type patterns. Order is precedence: more specific
# tags first so e.g. "Cantonese" wins over generic "Chinese" when both
# appear in a description.
#
# Each pattern is identity-anchored: an adjective in isolation
# ("Japanese") fires too aggressively because it might describe an
# ingredient ("Japanese whiskey") or technique ("Japanese knife").
# Instead we require the adjective to sit next to a venue-type noun
# (restaurant / eatery / spot / cuisine / kitchen / joint / pop-up /
# trattoria / etc.) OR for a strongly-typed venue noun to appear
# alone (izakaya, taqueria, bistro - those have only one possible
# reading).
#
# This reduces recall (some descriptions tag a place with the bare
# adjective only and fall through to NA), but eliminates the
# "ingredient-context" false positives that ruined the original
# pass.

# Helper: build a pattern that requires `adj` to appear adjacent to
# a venue-type noun. Word boundary on both sides.
.adj_with_venue <- function(adj) {
  venue_nouns <- paste(
    c("restaurant", "restaurants", "eatery", "eateries", "spot",
      "spots", "cuisine", "kitchen", "kitchens", "joint", "joints",
      "place", "places", "destination", "destinations", "pop[-\\s]?up",
      "pop[-\\s]?ups", "cafe", "house", "menu", "fare", "cooking",
      "diner", "deli", "tavern", "bar", "club", "shop"),
    collapse = "|"
  )
  paste0(
    "\\b(", adj, ")\\s+(", venue_nouns, ")\\b",
    "|",
    "\\b(", venue_nouns, ")\\s+(?:serves|serving|specializing\\s+in|focused\\s+on)\\s+",
    adj, "\\b",
    "|",
    "\\bserves?\\s+", adj, "\\b"
  )
}

.prose_cuisine_patterns <- list(
  # Strongly-typed venue nouns that have a single cuisine reading -
  # these can fire on their own.
  Cantonese       = "\\b(cantonese|dim\\s*sum\\s+(house|spot|restaurant)|cha\\s*chaan\\s*teng)\\b",
  Sichuan         = "\\b(sichuan|szechuan|chongqing)\\b",
  Japanese        = paste0(.adj_with_venue("japanese"),
                           "|\\b(izakaya|kaiseki|omakase\\s+(spot|counter|restaurant))\\b"),
  French          = paste0(.adj_with_venue("french"),
                           # "bistro" intentionally excluded - it's used
                           # generically for any casual American place
                           # ("bi-level bistro") and tagged non-French
                           # venues like Zuni. Brasserie/patisserie/
                           # boulangerie remain France-specific.
                           "|\\b(brasserie|patisserie|boulangerie)\\b"),
  Mexican         = paste0(.adj_with_venue("mexican|oaxacan"),
                           "|\\b(taqueria)\\b"),
  Italian         = paste0(.adj_with_venue("italian|tuscan|sicilian|roman"),
                           "|\\b(trattoria|osteria|enoteca)\\b"),
  Vietnamese      = .adj_with_venue("vietnamese"),
  Chinese         = .adj_with_venue("chinese"),
  Korean          = .adj_with_venue("korean"),
  Thai            = .adj_with_venue("thai"),
  Indian          = .adj_with_venue("indian"),
  Greek           = .adj_with_venue("greek|hellenic"),
  Mediterranean   = .adj_with_venue("mediterranean"),
  `Middle Eastern` = .adj_with_venue("middle\\s+eastern|lebanese|persian|iranian|turkish|israeli|syrian|palestinian"),
  Spanish         = paste0(.adj_with_venue("spanish|basque|catalan"),
                           "|\\b(tapas\\s+(bar|spot|restaurant))\\b"),
  Burmese         = .adj_with_venue("burmese"),
  Filipino        = .adj_with_venue("filipino|pinoy"),
  Ethiopian       = .adj_with_venue("ethiopian|eritrean"),
  Hawaiian        = .adj_with_venue("hawaiian|polynesian"),
  Californian     = paste0(.adj_with_venue("californian"),
                           "|\\bfarm[-\\s]to[-\\s]table\\b"),
  # "tasting menu" intentionally excluded - it's a format word used
  # across all fine-dining cuisines (kaiseki, Italian, French), not
  # an American-specific identity marker.
  `Contemporary American` = "\\b(new\\s+american|modern\\s+american|contemporary\\s+american)\\b",
  Seafood         = "\\b(seafood\\s+(restaurant|spot|joint|house)|oyster\\s+bar|raw\\s+bar)\\b",
  Steakhouse      = "\\b(steakhouse|chophouse)\\b",
  Pizza           = "\\b(pizzeria|pizza\\s+(joint|place|shop|spot)|neapolitan\\s+pizza|detroit[-\\s]style\\s+pizza)\\b",
  Ramen           = "\\b(ramen\\s+(shop|joint|spot|restaurant)|ramen-ya)\\b",
  `Bakery/Cafe`   = "\\b(bakery|patisserie|boulangerie|coffeehouse)\\b",
  Coffee          = "\\b(coffee\\s+(shop|bar|spot|joint)|specialty\\s+coffee|roaster)\\b",
  `Ice Cream`     = "\\b(ice\\s+cream\\s+(parlou?r|shop|spot|joint)|gelato\\s+shop)\\b"
)
