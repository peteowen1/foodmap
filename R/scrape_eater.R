#' Scrape Eater's "Essential X" + map guides
#'
#' Eater publishes city-by-city map guides ("The 38 Best Restaurants in
#' San Francisco", "Heatmap of New Restaurants", "Best Bars", etc.).
#' Each map page embeds the venue payload as inline JSON inside the
#' article HTML (one block per venue with `location.latitude`,
#' `location.longitude`, `name`, `venue.address`). We pull those out
#' with a couple of regexes -- no per-venue detail fetch needed.
#'
#' By default the scraper hits the city's flagship "Essential 38" /
#' "best restaurants" guide. Pass additional map slugs via
#' `extra_guides` to broaden across categories (best brunch, best
#' bars, best coffee, etc.).
#'
#' @param city Character. Lowercase city slug. Currently supported:
#'   `"san-francisco"`. Default `"san-francisco"`.
#' @param extra_guides Character vector of additional map slugs to
#'   fetch alongside the flagship list. Default `character()`.
#' @param use_cache Logical. Cache page responses for 24h. Default `FALSE`.
#'
#' @return A tibble with the standard scraper schema.
#' @export
scrape_eater <- function(city = "san-francisco",
                         extra_guides = character(),
                         use_cache = FALSE) {
  city <- validate_city_source(city, "eater")
  cli::cli_h1("Scraping Eater: {city}")

  base <- eater_base_url(city)
  guides <- unique(c(eater_default_guides(city), extra_guides))
  cli::cli_alert_info("Fetching {length(guides)} guide{?s}")

  results <- purrr::map(guides, function(slug) {
    url <- paste0(base, "/maps/", slug)
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
    eater_parse_guide(html_str, slug = slug)
  })
  results <- purrr::compact(results)
  if (length(results) == 0) {
    cli::cli_abort("No data scraped from any Eater guide.")
  }

  combined <- dplyr::bind_rows(results) |>
    dplyr::distinct(.data$name, .data$suburb, .keep_all = TRUE)
  cli::cli_alert_success("Found {nrow(combined)} venue{?s}")
  combined
}


#' Per-city Eater subdomain
#' @noRd
eater_base_url <- function(city) {
  switch(city,
    `san-francisco` = "https://sf.eater.com",
    cli::cli_abort("Unknown Eater city {.val {city}}")
  )
}

#' Default map slugs per city
#'
#' For SF: Essential 38, Heatmap (newest spots), brunch, pizza, steak,
#' and a few other key categories. Failures (404s) for individual
#' slugs are logged but don't abort the overall scrape.
#' @noRd
eater_default_guides <- function(city) {
  switch(city,
    `san-francisco` = c(
      "best-restaurants-san-francisco-38",
      "best-new-restaurants-san-francisco",
      "best-brunch-san-francisco",
      "best-pizza-san-francisco",
      "best-steakhouses-san-francisco",
      # Coffee / ice cream guides for cafe-side coverage. Eater SF
      # doesn't publish a separate bakery map (the existing "best
      # bakeries" slug 404s), so cafe coverage relies on coffee +
      # crossover from the Infatuation slugs above.
      "best-coffee-shops-san-francisco",
      "best-ice-cream-san-francisco"
    ),
    cli::cli_abort("No default Eater guides for {.val {city}}")
  )
}


#' Parse all venues out of an Eater map page's inline JSON
#'
#' @param html_str Raw HTML from an Eater map page.
#' @param slug Guide slug (e.g. `"best-pizza-san-francisco"`). When the
#'   slug implies a specific cuisine, every parsed row inherits that
#'   tag - Eater doesn't expose `servesCuisine` on its map pages, so
#'   this is the most reliable signal we have. NA for flagship lists
#'   without an implied cuisine.
#' @noRd
eater_parse_guide <- function(html_str, slug = NA_character_) {
  # Each venue's data is interleaved in the article HTML. The pattern is
  #   "location":{"latitude":X,"longitude":Y},"name":"NAME"
  # followed (within ~3 KB) by
  #   "venue":{...,"address":"ADDR"...}
  loc_re <- '"location":\\{"latitude":(-?[0-9.]+),"longitude":(-?[0-9.]+)\\},"name":"([^"]+)"'
  m <- stringr::str_match_all(html_str, loc_re)[[1]]
  if (nrow(m) == 0) return(NULL)

  starts <- stringr::str_locate_all(html_str, loc_re)[[1]][, "start"]
  cuisine_from_slug <- eater_slug_to_cuisine(slug)

  rows <- lapply(seq_len(nrow(m)), function(i) {
    name <- eater_unescape(m[i, 4])
    lat  <- suppressWarnings(as.numeric(m[i, 2]))
    lng  <- suppressWarnings(as.numeric(m[i, 3]))

    chunk_start <- starts[i]
    chunk_end   <- min(nchar(html_str), chunk_start + 3000L)
    chunk       <- substr(html_str, chunk_start, chunk_end)

    addr <- stringr::str_match(chunk, '"address":"([^"]+)"')[1, 2]
    if (!is.na(addr)) addr <- eater_unescape(addr)

    suburb <- eater_suburb_from_address(addr)

    # Pull the venue slug out of the same chunk, then locate the
    # editorial article block (anchored at id="<slug>") and extract
    # the description + price-range paragraphs that Eater renders
    # underneath the H2.
    venue_slug <- stringr::str_match(chunk, '"slug":"([^"]+)"')[1, 2]
    article    <- eater_extract_article_block(html_str, venue_slug)

    # When the slug didn't imply a cuisine (flagship best-of lists),
    # try the editorial description: Eater's writers routinely tag a
    # place as "Italian", "Cantonese-style", "Detroit-style pizza"
    # etc. via the shared prose_to_cuisine() rules.
    cuisine_final <- if (is.na(cuisine_from_slug)) {
      prose_to_cuisine(article$description)
    } else {
      cuisine_from_slug
    }

    tibble::tibble(
      name         = name,
      suburb       = suburb,
      address      = addr,
      cuisine      = cuisine_final,
      category     = "Restaurant",
      description  = article$description,
      price_range  = article$price_range,
      rating       = NA_real_,
      rating_scale = NA_character_,
      latitude     = lat,
      longitude    = lng,
      url          = NA_character_
    )
  })

  dplyr::bind_rows(rows)
}


#' Extract the description + price range from the editorial block
#'
#' Eater articles render each venue as:
#'   <a id="<slug>">...<h2>Name</h2>
#'   <p><strong>Open for:</strong> ...</p>
#'   <p><strong>Price range:</strong> $$$</p>
#'   <p>Free-prose description...</p>
#'
#' Given the venue slug, find that block and return both the price
#' range as an integer (count of `$`) and the first non-metadata
#' paragraph as the description.
#' @noRd
eater_extract_article_block <- function(html_str, slug) {
  empty <- list(description = NA_character_, price_range = NA_integer_)
  if (is.na(slug) || !nzchar(slug)) return(empty)

  anchor <- paste0('id="', slug, '"')
  pos <- stringr::str_locate(html_str, stringr::fixed(anchor))[1, "start"]
  if (is.na(pos)) return(empty)

  # 5 KB chunk should cover the H2 + a few <p> blocks comfortably; the
  # next venue's anchor is typically further down.
  block <- substr(html_str, pos, min(nchar(html_str), pos + 5000L))

  # All <p>...</p> chunks within the block, including their inner HTML
  # so we can spot <strong>Price range</strong> markers.
  paragraphs <- stringr::str_match_all(
    block, "<p[^>]*>([\\s\\S]*?)</p>"
  )[[1]]
  if (nrow(paragraphs) == 0) return(empty)

  # Price range: the paragraph beginning with "Price range:". Count $.
  price_int <- NA_integer_
  for (k in seq_len(nrow(paragraphs))) {
    inner <- paragraphs[k, 2]
    if (grepl("Price range:", inner, fixed = TRUE)) {
      n_dollar <- stringr::str_count(inner, "\\$")
      if (n_dollar >= 1) price_int <- as.integer(min(n_dollar, 4))
      break
    }
  }

  # Description: first paragraph that isn't a metadata row. Eater
  # uses several formats: "<strong>Open for:</strong> ...",
  # "<b>Open for</b>: ...", or even plain "Open for: ..." with no
  # wrapper. Detect them all by looking for a metadata label near the
  # paragraph's start, regardless of HTML tags.
  meta_labels <- "(?i)\\b(open for|price range|address|reservations|recommended dish(es)?|why we like it|why we love it|hours|phone|website)\\b\\s*:"
  desc <- NA_character_
  for (k in seq_len(nrow(paragraphs))) {
    inner <- paragraphs[k, 2]
    text <- inner |>
      stringr::str_replace_all("<[^>]+>", "") |>
      eater_unescape() |>
      stringr::str_squish()
    if (nchar(text) < 30) next
    # First ~40 chars contain a metadata label? Skip.
    head <- substr(text, 1, 40)
    if (grepl(meta_labels, head, perl = TRUE)) next
    desc <- if (nchar(text) > 500) {
      paste0(substr(text, 1, 497), "...")
    } else {
      text
    }
    break
  }

  list(description = desc, price_range = price_int)
}


#' Extract the suburb (city/neighborhood) from an Eater address string
#'
#' Eater addresses look like "2700 Jones St, San Francisco, CA, 94133, US".
#' We return the second-to-last comma section before the state -- which
#' is typically the city or neighborhood (e.g. "Presidio" for Dalida,
#' "San Francisco" for most). Heuristic but robust for SF data.
#' @noRd
eater_suburb_from_address <- function(addr) {
  if (is.na(addr) || !nzchar(addr)) return(NA_character_)
  parts <- stringr::str_split(addr, ",\\s*")[[1]]
  parts <- parts[nzchar(parts)]
  # strip trailing US country / postcode / 2-letter state
  while (length(parts) > 0 &&
         (grepl("^[A-Z]{2}$|^US$|^USA$|^\\d+$", parts[length(parts)]) ||
          parts[length(parts)] == "United States")) {
    parts <- parts[-length(parts)]
  }
  if (length(parts) == 0) return(NA_character_)
  parts[length(parts)]
}


#' Map an Eater guide slug to an implied cuisine tag
#'
#' Eater's map pages don't carry `servesCuisine`, so the slug is the
#' most reliable cuisine signal: `best-pizza-san-francisco` is, by
#' construction, a list of pizza places. Returns NA for flagship
#' best-of-everything lists where no single cuisine is implied.
#' @noRd
eater_slug_to_cuisine <- function(slug) {
  if (is.na(slug) || !nzchar(slug)) return(NA_character_)
  # Order matters: more specific first
  patterns <- c(
    "ice-cream"    = "Ice Cream",
    "coffee"       = "Coffee",
    "pizza"        = "Pizza",
    "brunch"       = "Brunch",
    "steakhouse"   = "Steakhouse",
    "ramen"        = "Ramen",
    "pho"          = "Vietnamese",
    "noodle"       = "Noodles",
    "pasta"        = "Pasta",
    "dim-sum"      = "Dim Sum",
    "italian"      = "Italian",
    "chinese"      = "Chinese",
    "japanese"     = "Japanese",
    "korean"       = "Korean",
    "vietnamese"   = "Vietnamese",
    "thai"         = "Thai",
    "indian"       = "Indian",
    "mexican"      = "Mexican",
    "french"       = "French",
    "seafood"      = "Seafood",
    "burger"       = "Burgers",
    "barbecue"     = "Barbecue",
    "bbq"          = "Barbecue",
    "bakeries"     = "Bakery/Cafe",
    "bagel"        = "Bakery/Cafe"
  )
  for (key in names(patterns)) {
    if (grepl(key, slug, fixed = TRUE)) return(patterns[[key]])
  }
  NA_character_
}


#' Decode the Eater-flavoured escapes (Unicode + backslash JSON)
#' @noRd
eater_unescape <- function(x) {
  if (is.na(x)) return(x)
  # Replace \uXXXX escapes with the literal char
  x <- stringi::stri_unescape_unicode(x)
  decode_html_entities(x)
}
