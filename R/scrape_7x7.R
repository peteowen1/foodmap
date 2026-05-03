#' Scrape 7x7's "Big Eat" / "Big Cheap" SF dish lists
#'
#' 7x7 publishes annual SF dish-centric editorial lists - "The Big
#' Eat" (100 must-eat dishes), "The Big Cheap" (49 cheap eats). Each
#' entry is one dish at one restaurant. The pages format every entry
#' as a numbered paragraph:
#'
#'     N. <dish> at <restaurant>, <address> (<neighborhood>), <website>
#'
#' which we extract via regex - no per-venue detail fetch.
#'
#' By default the scraper hits both the latest Big Eat (2021, the
#' most recent SF edition; older but still mostly relevant venues)
#' and the latest Big Cheap (2026). Pass extra URLs via
#' `extra_guides` to broaden coverage.
#'
#' @param city Character. Currently only `"san-francisco"`. Default
#'   `"san-francisco"`.
#' @param extra_guides Character vector of extra 7x7 article URLs.
#' @param use_cache Logical. Cache page responses for 24h.
#'
#' @return A tibble with the standard scraper schema.
#' @export
scrape_7x7 <- function(city = "san-francisco",
                       extra_guides = character(),
                       use_cache = FALSE) {
  city <- validate_city_source(city, "7x7")
  cli::cli_h1("Scraping 7x7: {city}")

  guides <- unique(c(seven_default_guides(city), extra_guides))
  cli::cli_alert_info("Fetching {length(guides)} guide{?s}")

  results <- purrr::map(guides, function(url) {
    cli::cli_alert_info("  {.url {url}}")
    Sys.sleep(RATE_LIMIT_SECS)
    html_str <- tryCatch(
      cached_fetch(url, use_cache = use_cache),
      error = function(e) {
        cli::cli_warn("  failed: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(html_str)) return(NULL)
    seven_parse_guide(html_str)
  })
  results <- purrr::compact(results)
  if (length(results) == 0) {
    cli::cli_abort("No data scraped from any 7x7 guide.")
  }

  combined <- dplyr::bind_rows(results) |>
    dplyr::distinct(.data$name, .data$suburb, .keep_all = TRUE)
  cli::cli_alert_success("Found {nrow(combined)} venue{?s}")
  combined
}


#' Default 7x7 guide URLs per city
#' @noRd
seven_default_guides <- function(city) {
  switch(city,
    `san-francisco` = c(
      "https://www.7x7.com/big-eat-2021-best-restaurant-takeout-san-francisco-2650835710.html",
      "https://www.7x7.com/49-cheap-things-san-francisco-2676758041.html"
    ),
    cli::cli_abort("No default 7x7 guides for {.val {city}}")
  )
}


#' Parse all venues out of a 7x7 dish-list page
#'
#' Each entry follows the pattern:
#'   "N. <dish> at <restaurant>, <address> (<neighborhood>), <website>"
#' The regex is permissive about trailing fields and tolerant of stray
#' commas inside dish or restaurant names.
#' @noRd
seven_parse_guide <- function(html_str) {
  page <- rvest::read_html(html_str)
  paragraphs <- rvest::html_elements(page, "p") |> rvest::html_text2()

  # Pattern: N. <dish> at <restaurant>, <address> (<neighborhood>)
  # The leading number is mandatory; the comma after restaurant is the
  # split point we anchor on; everything up to the first "(" is address.
  pattern <- paste0(
    "^\\s*\\d+\\.\\s+",
    "(.+?)",                     # dish
    "\\s+at\\s+",
    "(.+?),\\s+",                # restaurant name
    "([^()]+?)\\s*",             # address
    "\\(([^)]+)\\)"              # neighborhood
  )

  rows <- list()
  for (p in paragraphs) {
    m <- stringr::str_match(p, pattern)
    if (is.na(m[1, 1])) next
    dish      <- stringr::str_squish(m[1, 2])
    rest_name <- stringr::str_squish(m[1, 3])
    address   <- stringr::str_squish(m[1, 4])
    suburb    <- stringr::str_squish(m[1, 5])
    if (!nzchar(rest_name)) next

    # 7x7's "Big Cheap" listicles inject pricing / hours after the
    # venue name, e.g. "Toy Soldier. // $1 each, 11am through 9pm".
    # The greedy regex pulls all of that into the rest_name capture.
    # Trim everything from the first `//` or `. <prose>` onward so we
    # keep just the real venue name.
    rest_name <- seven_clean_venue_name(rest_name)
    if (nchar(rest_name) < 2 || nchar(rest_name) > 60) next

    # Reject rows where the captured "address" is obviously prose
    # (times, dollar amounts, "until they run out!", etc.) -- the
    # whole row is a Big-Cheap listing whose format we don't model.
    if (looks_like_prose(address)) next

    rows[[length(rows) + 1]] <- tibble::tibble(
      name         = rest_name,
      suburb       = suburb,
      address      = paste(address, suburb, sep = ", "),
      cuisine      = seven_dish_to_cuisine(dish),
      category     = "Restaurant",
      description  = if (nzchar(dish)) dish else NA_character_,
      price_range  = NA_integer_,
      rating       = NA_real_,
      rating_scale = NA_character_,
      latitude     = NA_real_,
      longitude    = NA_real_,
      url          = NA_character_
    )
  }

  if (length(rows) == 0) return(NULL)
  dplyr::bind_rows(rows)
}


#' Strip trailing prose/pricing chunks from a 7x7 venue name capture
#'
#' "Toy Soldier. // $1 each" -> "Toy Soldier"
#' "Mister Jiu's" -> "Mister Jiu's" (no change)
#' Conservative: only splits on `//` or `. $` (period-space-dollar) to
#' avoid eating real-name punctuation like "Sons & Daughters."
#' @noRd
seven_clean_venue_name <- function(name) {
  if (is.na(name) || !nzchar(name)) return(name)
  # Cut at the first `//` (Big Cheap separator)
  name <- stringr::str_split_fixed(name, "\\s*//", 2)[1, 1]
  # Cut at ". $" (period followed by money), e.g. "Toy Soldier. $1"
  name <- stringr::str_split_fixed(name, "\\.\\s+\\$", 2)[1, 1]
  # Drop any single trailing period that's now dangling
  name <- stringr::str_replace(name, "\\.\\s*$", "")
  stringr::str_squish(name)
}


#' Heuristic: does the captured "address" look like marketing prose?
#'
#' Real addresses contain street numbers + street words. 7x7's Big
#' Cheap items frequently feed back things like "11am through 9pm" or
#' "$5 every other Tuesday" through the address slot. We reject those
#' rather than trying to geocode them.
#' @noRd
looks_like_prose <- function(s) {
  if (is.na(s) || !nzchar(s)) return(FALSE)
  # Time-of-day attached to a digit doesn't carry a word boundary
  # ("11am" - digit-to-letter is word-to-word, no `\b`), so match the
  # numeric prefix explicitly. Other markers want word boundaries.
  prose_markers <- paste(
    "(?i)\\d+\\s*(am|pm)\\b",                                 # times
    "(?i)\\b(until|every|each)\\b",                           # filler words
    "(?i)\\b(monday|tuesday|wednesday|thursday|friday|saturday|sunday)\\b",
    "\\$",                                                     # money
    sep = "|"
  )
  grepl(prose_markers, s, perl = TRUE)
}


#' Infer cuisine from a 7x7 dish description
#'
#' 7x7's "Big Eat" / "Big Cheap" lists are dish-centric ("Phở at So-and-
#' so..."), so the dish text is a strong cuisine signal. This helper
#' runs a keyword dictionary - more specific cuisines first (Pizza
#' before Italian; Ramen before Japanese) so the right tag wins when
#' multiple keywords appear. Returns NA when no keyword fires; the
#' downstream filter then hides the venue under any narrowed cuisine
#' filter rather than guessing.
#' @noRd
seven_dish_to_cuisine <- function(dish) {
  if (is.na(dish) || !nzchar(dish)) return(NA_character_)
  # Normalise to lowercase ASCII so accented characters (Phở, banh mì,
  # café) collapse to plain English forms - keeps the patterns readable
  # and dodges Unicode quirks in R's regex character classes.
  text <- tolower(dish)
  text <- tryCatch(
    stringi::stri_trans_general(text, "Latin-ASCII"),
    error = function(e) text
  )
  # Order matters: more specific tags first, then general fallbacks.
  # Cantonese sits above Vietnamese because pork "bao buns" / "char
  # siu buns" should win over the Vietnamese "bun" vermicelli reading.
  patterns <- list(
    Pizza         = "\\b(pizza|focaccia)\\b",
    Ramen         = "\\b(ramen|tsukemen)\\b",
    Burgers       = "\\b(burger|cheeseburger|smashburger|nightburger)\\b",
    Cantonese     = "\\b(dim\\s*sum|char\\s*siu|har\\s*gow|congee|xiao\\s*long\\s*bao|cha\\s*chaan|wonton|siu\\s*mai|peking|baoz?|bao\\s*bun|mapo|sichuan|szechuan|dumplings?|hot\\s*pot)\\b",
    Vietnamese    = "\\b(pho|banh\\s*mi|goi\\s*cuon|com\\s*tam|vermicelli|bun\\s*(bo|cha|rieu))\\b",
    Japanese      = "\\b(sushi|sashimi|udon|donburi|omakase|yakitori|gyoza|tonkatsu|tempura|izakaya|onigiri|katsu|chirashi)\\b",
    Korean        = "\\b(bibimbap|bulgogi|kimchi|japchae|kalbi(jjim)?|banchan|gochu|kimbap|namul)\\b",
    Thai          = "\\b(pad\\s*thai|tom\\s*yum|larb|som\\s*tam|satay|panang|massaman|khao\\s*soi)\\b",
    Indian        = "\\b(curry|tikka|masala|biryani|naan|dosa|samosa|paneer|vindaloo|chaat|saag)\\b",
    Mexican       = "\\b(taco|burrito|tamale|mole|enchilada|al\\s*pastor|tostada|tortilla|quesadilla|carnitas|pozole|tinga|sope|sopes|quesabirria|churros?)\\b",
    `Middle Eastern` = "\\b(hummus|falafel|shawarma|kebab|baba\\s*ganoush|tahini|labneh|manakish|mezze|manti)\\b",
    Mediterranean = "\\b(moussaka|spanakopita|tzatziki|souvlaki|dolma|gyro|paella)\\b",
    French        = "\\b(croissant|baguette|souffle|escargot|foie\\s*gras|coq\\s*au\\s*vin|cassoulet|beignet|kouign\\s*amann|croque|pain\\s*au\\s*chocolat|canele|tarte\\s*tatin|pate|petit\\s*four)\\b",
    Pasta         = "\\b(pasta|risotto|raviolo|ravioli|lasagna|gnocchi|carbonara|bolognese|tagliatelle|cacio|spaghetti|spaghettoni|spaghittusu|linguine|pappardelle|orecchiette|parm(esan)?)\\b",
    Italian       = "\\b(porchetta|prosciutto|tiramisu|polenta|arancini|antipasti|negroni)\\b",
    Steakhouse    = "\\b(steak|filet|ribeye|tomahawk)\\b",
    Seafood       = "\\b(oyster|crab|lobster|cioppino|scallop|shrimp|prawn|tuna|salmon|ceviche|chowder|clam)\\b",
    Barbecue      = "\\b(brisket|smoked\\s+(meat|pork|beef|ribs)|bbq|barbecue)\\b",
    Sandwiches    = "\\b(sandwich|hoagie|sando|sub|grinder|panini|reuben)\\b",
    `Bakery/Cafe` = "\\b(pastry|bread|biscuit|donut|doughnut|cookie|cake|tart|scone|bagel|strudel|toast)\\b",
    Coffee        = "\\b(coffee|espresso|latte|cappuccino|cortado|macchiato|flat\\s+white)\\b",
    `Ice Cream`   = "\\b(ice\\s*cream|gelato|sorbet|sundae|soft\\s*serve)\\b",
    American      = "\\b(fried\\s+chicken|wings|hot\\s*dog|mac\\s*(and|&|n)\\s*cheese|cornbread|grits|gumbo|jambalaya|po\\s*boy)\\b"
  )
  for (cuisine in names(patterns)) {
    if (grepl(patterns[[cuisine]], text, perl = TRUE)) return(cuisine)
  }
  NA_character_
}
