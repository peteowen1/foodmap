#' Scrape the Hale 'Aina Awards from Honolulu Magazine
#'
#' The Hale 'Aina Awards are HONOLULU Magazine's annual reader-voted
#' restaurant awards - the local "people's choice" equivalent of the
#' Beard Awards. The master winners article publishes Gold / Silver /
#' Bronze / Finalist picks across ~40 categories (best new restaurant,
#' best izakaya, best service, best cocktail program, etc.).
#'
#' The page is server-rendered with a strict
#' `<h3>Category</h3><p>RANK - <strong>NAME</strong></p>...` shape,
#' so a regex over `<h3>` blocks plus the following `<p>` lines is
#' enough to pull the full winner list without a headless browser.
#'
#' Outer-island categories ("Best Maui Restaurant", "Best Hawai'i
#' Island Restaurant", "Best Kaua'i Restaurant") are skipped because
#' those venues are non-day-trippable from Honolulu and would be
#' filtered out by the geocoder's city bbox anyway. Categories that
#' name a person rather than a venue ("Restaurateur of the Year")
#' are also skipped.
#'
#' @param city Character. Currently `"honolulu"`. Default
#'   `"honolulu"`.
#' @param use_cache Logical. Cache the master article HTML for 24h.
#'   Default `FALSE`.
#'
#' @return A tibble with the standard scraper schema. The
#'   `description` field captures the category and rank
#'   ("Hale 'Aina 2025 GOLD - Best Izakaya").
#' @export
scrape_hale_aina <- function(city = "honolulu", use_cache = FALSE) {
  city <- validate_city_source(city, "hale_aina")
  cli::cli_h1("Scraping Hale \u02BBAina Awards: {city}")

  url <- hale_aina_url(city)
  cli::cli_alert_info("Fetching {.url {url}}")
  Sys.sleep(RATE_LIMIT_SECS)
  html_str <- cached_fetch(url, use_cache = use_cache)

  records <- hale_aina_parse(html_str)
  if (nrow(records) == 0) {
    cli::cli_abort(
      "No Hale \u02BBAina winners parsed - has the article markup changed?"
    )
  }

  # One row per (name, category, rank) - keep all so the same venue
  # winning two categories appears with both descriptions, then dedup
  # by name keeping the highest rank (gold > silver > bronze > finalist).
  records <- records[order(records$rank_order), ]
  result <- records[!duplicated(records$name), , drop = FALSE]

  cli::cli_alert_success("Found {nrow(result)} venue{?s}")
  hale_aina_to_tibble(result, url)
}


#' Master article URL per city
#' @noRd
hale_aina_url <- function(city) {
  switch(city,
    honolulu = paste0(
      "https://www.honolulumagazine.com/",
      "hale-aina-award-winners-the-best-restaurants-in-hawaii/"
    ),
    cli::cli_abort("No Hale \u02BBAina URL configured for {.val {city}}")
  )
}


#' Extract winners from the master article HTML
#'
#' Walks the article in document order: each `<h3>` is a category, and
#' every `<p>RANK - <strong>...</strong></p>` row that follows it (up to
#' the next `<h3>` or `<h2>`) is a winner at that rank. The strong tag
#' wraps either a bare name or an `<a>NAME</a>` link; we pull the
#' visible text out of whichever shape we hit.
#'
#' Returns a tibble with `name`, `category`, `rank`, `rank_order`,
#' `award_year`. Outer-island and person-name categories are filtered
#' out before parsing rows.
#' @noRd
hale_aina_parse <- function(html_str) {
  # Constrain parsing to the entry-content block so navigation H3s
  # ("More Honolulu Magazine") don't leak in.
  body_match <- stringr::str_match(
    html_str,
    "<h1 class=\"entry-title\"[\\s\\S]*?(<h2>Related|<h2>Most Popular)"
  )
  body <- if (!is.na(body_match[1, 1])) body_match[1, 1] else html_str

  year <- hale_aina_extract_year(body)

  # Tokenise into a sequence of (kind, content) blocks: h3 sections
  # and p (paragraph) entries. We use str_match_all for both then
  # interleave by position.
  pattern <- "<(h3|h2|p)[^>]*>([\\s\\S]*?)</\\1>"
  matches <- stringr::str_match_all(body, pattern)[[1]]
  starts  <- stringr::str_locate_all(body, pattern)[[1]][, "start"]
  if (nrow(matches) == 0) {
    return(empty_hale_aina_records())
  }

  records <- list()
  current_category <- NA_character_

  for (i in seq_len(nrow(matches))) {
    kind <- matches[i, 2]
    inner <- matches[i, 3]

    if (kind == "h3" || kind == "h2") {
      cat_text <- strip_html_tags(inner) |> stringr::str_squish()
      if (hale_aina_is_skip_category(cat_text)) {
        current_category <- NA_character_
      } else {
        current_category <- cat_text
      }
      next
    }

    # paragraph - only interesting when we're inside a tracked category
    if (is.na(current_category)) next

    parsed <- hale_aina_parse_paragraph(inner)
    if (is.null(parsed)) next
    parsed$category <- current_category
    parsed$award_year <- year
    records[[length(records) + 1]] <- parsed
  }

  if (length(records) == 0) return(empty_hale_aina_records())
  dplyr::bind_rows(records)
}


#' Parse a single winner paragraph
#'
#' Expected shape: `GOLD - <strong>NAME</strong>` (with name possibly
#' wrapped in `<a>...</a>`). Some rows nest the link outside the strong
#' tag (`<a><strong>NAME</strong></a>`), so we extract by stripping
#' all tags after locating the strong block.
#'
#' Returns `NULL` for paragraphs that don't match the rank pattern
#' (intro lines, sidebar links, etc.).
#' @noRd
hale_aina_parse_paragraph <- function(inner) {
  rank_match <- stringr::str_match(
    inner,
    "^(?i)\\s*(?:<[^>]+>\\s*)*(GOLD|SILVER|BRONZE|FINALIST)"
  )
  rank <- rank_match[1, 2]
  if (is.na(rank)) return(NULL)
  rank <- toupper(rank)

  # Pull the visible text inside the first <strong>...</strong>
  strong_match <- stringr::str_match(inner, "<strong[^>]*>([\\s\\S]*?)</strong>")
  if (is.na(strong_match[1, 2])) return(NULL)
  name <- strip_html_tags(strong_match[1, 2]) |>
    decode_html_entities() |>
    stringr::str_squish()
  if (!nzchar(name) || nchar(name) > 80) return(NULL)
  # Strip any leftover " By Author" or trailing tags. The character
  # class catches both ASCII hyphen and the en dash (U+2013) HONOLULU
  # Magazine uses in winner paragraphs.
  name <- stringr::str_replace(name, "\\s*[\u2013-]\\s*.*$", "")
  if (!nzchar(name)) return(NULL)

  tibble::tibble(
    name = name,
    rank = rank,
    rank_order = switch(rank, GOLD = 1L, SILVER = 2L, BRONZE = 3L, FINALIST = 4L, 5L)
  )
}


#' Categories to ignore when building the winner list
#'
#' Three kinds of skip:
#'   1. Outer islands (Maui / Big Island / Kaua'i) - non-day-trippable
#'   2. People-not-places (Restaurateur of the Year)
#'   3. Section dividers ("The rest of our readers' picks:")
#' @noRd
hale_aina_is_skip_category <- function(cat) {
  if (is.na(cat) || !nzchar(cat)) return(TRUE)
  skip_patterns <- c(
    "(?i)best\\s+maui\\s+restaurant",
    "(?i)best\\s+(hawai)",       # "Best Hawai'i Island Restaurant"
    "(?i)best\\s+kaua",
    "(?i)restaurateur\\s+of\\s+the\\s+year",
    "(?i)the\\s+rest\\s+of",
    "(?i)^related$",
    "(?i)^most\\s+popular$",
    "(?i)^promotional\\s+content$",
    # The hero/teaser section above the readers' picks repeats the
    # gold-finalist for a handful of categories - lock the parser onto
    # the structured "The rest of our readers' picks" block by skipping
    # everything before it via a section toggle. The "Old-School
    # Restaurant We Love" hero category is captured under the broader
    # readers'-picks section that follows.
    "(?i)^old-school\\s+restaurant\\s+we\\s+love$"
  )
  any(vapply(skip_patterns, function(p) grepl(p, cat, perl = TRUE), logical(1)))
}


#' Pull the award year out of the article H1
#'
#' H1 reads "2025 Hale 'Aina Award Winners: ..." - grab the leading
#' four-digit year. Falls back to the current calendar year if the
#' headline format ever changes.
#' @noRd
hale_aina_extract_year <- function(html_str) {
  m <- stringr::str_match(
    html_str, "<h1[^>]*>\\s*([0-9]{4})\\s+Hale"
  )
  year <- suppressWarnings(as.integer(m[1, 2]))
  if (is.na(year)) {
    year <- as.integer(format(Sys.Date(), "%Y"))
  }
  year
}


#' Strip all HTML tags from a string
#' @noRd
strip_html_tags <- function(x) {
  if (is.na(x)) return(x)
  stringr::str_replace_all(x, "<[^>]+>", "")
}


#' Empty record tibble matching the parse output schema
#' @noRd
empty_hale_aina_records <- function() {
  tibble::tibble(
    name = character(),
    rank = character(),
    rank_order = integer(),
    category = character(),
    award_year = integer()
  )
}


#' Convert parsed records to the package's standard scraper schema
#'
#' Maps each award category text ("Best Cocktail Bar", "Best Coffee
#' Shop", "Best Bakery") to a standardised category column so the
#' downstream mix-by-category analysis sees Hale Aina's bars and cafes
#' as such instead of all collapsing into Restaurant. Cuisine inherits
#' the same mapping where the award implies one (Coffee, Cafe, Bar).
#' @noRd
hale_aina_to_tibble <- function(records, url) {
  classified <- vapply(records$category, hale_aina_classify_award,
                       character(2),
                       USE.NAMES = FALSE)
  category_col <- classified[1, ]
  cuisine_col  <- classified[2, ]
  cuisine_col[!nzchar(cuisine_col)] <- NA_character_

  dplyr::transmute(
    records,
    name         = .data$name,
    suburb       = NA_character_,
    address      = NA_character_,
    cuisine      = cuisine_col,
    category     = category_col,
    description  = paste0(
      "Hale \u02BBAina ", .data$award_year, " ", .data$rank,
      " - ", .data$category
    ),
    price_range  = NA_integer_,
    rating       = NA_real_,
    rating_scale = NA_character_,
    latitude     = NA_real_,
    longitude    = NA_real_,
    url          = url
  )
}


#' Map a Hale Aina award category text to a (category, cuisine) pair
#'
#' Returns a length-2 character vector. The category column on the
#' final tibble follows the standard cafe / bar / restaurant axis;
#' cuisine is the more specific bar/cafe sub-type when the award
#' implies one. Empty string in cuisine means "leave as NA".
#'
#' Award patterns observed on Hale Aina articles:
#'   * "Cocktail Bar", "Bar", "Brewery", "Tiki Bar"     -> Bar
#'   * "Coffee Shop", "Coffee", "Cafe"                  -> Cafe
#'   * "Bakery", "Pastry"                               -> Cafe
#'   * "Brunch", "Breakfast"                            -> Cafe
#'   * everything else (sushi, steak, ramen, etc.)      -> Restaurant
#' @noRd
hale_aina_classify_award <- function(award) {
  if (is.null(award) || is.na(award) || !nzchar(award)) {
    return(c("Restaurant", ""))
  }
  a <- tolower(award)
  # Bar paths (most specific first)
  if (grepl("cocktail", a))           return(c("Bar", "Cocktail Bar"))
  if (grepl("brewery|brewpub", a))    return(c("Bar", "Brewery"))
  if (grepl("tiki",        a))        return(c("Bar", "Tiki Bar"))
  if (grepl("wine bar",    a))        return(c("Bar", "Wine Bar"))
  if (grepl("(^|\\s)bar(\\s|$)", a))  return(c("Bar", "Bar"))
  # Cafe paths
  if (grepl("coffee",      a))        return(c("Cafe", "Coffee"))
  if (grepl("bakery|patisserie|pastry", a)) return(c("Cafe", "Bakery"))
  if (grepl("brunch|breakfast", a))   return(c("Cafe", "Breakfast"))
  if (grepl("(^|\\s)cafe(\\s|$)", a)) return(c("Cafe", "Cafe"))
  c("Restaurant", "")
}
