# Internal helpers --------------------------------------------------------

#' Default pause between HTTP requests (seconds)
#' @noRd
RATE_LIMIT_SECS <- 0.2

#' Broadsheet hotlist URL for a given city
#' @noRd
broadsheet_url <- function(city = "sydney") {
  city <- validate_city_source(city, "broadsheet")
  glue::glue("https://www.broadsheet.com.au/hotlist/{city}")
}

#' Realistic browser User-Agent string
#' @noRd
user_agent_string <- function() {
  paste0(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) ",
    "Chrome/131.0.0.0 Safari/537.36"
  )
}

#' Get Google Places API key from argument or env var
#' @noRd
resolve_api_key <- function(api_key = NULL) {
  key <- api_key %||% Sys.getenv("GOOGLE_PLACES_API_KEY", unset = "")
  if (nchar(key) == 0) {
    cli::cli_abort(c(
      "No Google Places API key found.",
      "i" = "Set {.envvar GOOGLE_PLACES_API_KEY} or pass {.arg api_key}."
    ))
  }
  key
}

#' Escape text for safe XML embedding
#' @noRd
xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&apos;", x, fixed = TRUE)
  x
}

#' Format a price range integer (1-4) as dollar signs
#' @noRd
format_price <- function(price_range) {
  ifelse(
    is.na(price_range) | price_range == 0,
    "",
    strrep("$", price_range)
  )
}

#' Known cuisine names used by Good Food Guide for tag filtering and fallback
#' @noRd
CUISINE_NAMES <- c(
  "contemporary", "modern australian", "italian", "french", "japanese",
  "chinese", "thai", "indian", "korean", "vietnamese", "greek",
  "mexican", "middle eastern", "asian", "seafood", "european",
  "mediterranean", "pub dining", "cantonese", "sichuan", "malaysian",
  "indonesian", "turkish", "lebanese", "spanish", "portuguese",
  "american", "pizza", "pasta", "sushi", "ramen", "yum cha",
  "sri lankan", "nepalese", "filipino", "peruvian", "argentinian",
  "brazilian", "african", "ethiopian", "moroccan", "persian",
  "cafe", "bar"
)

#' All valid scraper source names
#' @noRd
valid_sources <- function() {
  c("broadsheet", "gourmet_traveller", "timeout", "urban_list", "agfg",
    "good_food_guide", "gfg_awards", "concrete_playground", "infatuation")
}

#' Cities supported by a given source
#' @noRd
supported_cities_for_source <- function(source) {
  switch(source,
    broadsheet          = c("sydney", "melbourne", "brisbane", "adelaide", "perth", "hobart"),
    gourmet_traveller   = c("sydney", "melbourne"),
    timeout             = c("sydney", "melbourne"),
    urban_list          = c("sydney", "melbourne"),
    agfg                = c("sydney", "melbourne", "brisbane", "adelaide", "perth",
                            "hobart", "canberra", "darwin", "gold-coast"),
    good_food_guide     = c("sydney", "melbourne"),
    gfg_awards          = "sydney",
    concrete_playground = c("sydney", "melbourne"),
    infatuation         = "san-francisco",
    cli::cli_abort("Unknown source: {.val {source}}")
  )
}

#' Validate city is supported for a given source
#' @noRd
validate_city_source <- function(city, source) {
  city <- tolower(city)
  supported <- supported_cities_for_source(source)
  if (!city %in% supported) {
    cli::cli_abort(
      "City {.val {city}} is not supported for {.val {source}}. Choose from: {.val {supported}}."
    )
  }
  city
}

#' Decode common HTML entities found in scraped JSON-LD payloads
#'
#' Concrete Playground and The Infatuation both encode apostrophes,
#' ampersands and quotes as HTML entities inside their JSON-LD strings
#' (e.g. `Emmy&apos;s Spaghetti Shack`). This helper handles the few
#' that come up in practice without pulling in an XML/HTML parser
#' just for entity decoding.
#' @noRd
decode_html_entities <- function(x) {
  if (is.na(x)) return(x)
  x |>
    gsub("&#039;", "'",  x = _, fixed = TRUE) |>
    gsub("&#39;",  "'",  x = _, fixed = TRUE) |>
    gsub("&apos;", "'",  x = _, fixed = TRUE) |>
    gsub("&amp;",  "&",  x = _, fixed = TRUE) |>
    gsub("&quot;", "\"", x = _, fixed = TRUE) |>
    gsub("&lt;",   "<",  x = _, fixed = TRUE) |>
    gsub("&gt;",   ">",  x = _, fixed = TRUE)
}

#' Build an empty restaurant tibble with the standard schema
#' @noRd
empty_restaurant_tibble <- function() {
  tibble::tibble(
    name         = character(),
    suburb       = character(),
    address      = character(),
    cuisine      = character(),
    category     = character(),
    description  = character(),
    price_range  = integer(),
    rating       = double(),
    rating_scale = character(),
    latitude     = double(),
    longitude    = double(),
    url          = character()
  )
}
