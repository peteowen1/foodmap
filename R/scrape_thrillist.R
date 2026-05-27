#' Scrape Thrillist's "Best Restaurants in CITY" editorial roundup
#'
#' Thrillist publishes a single canonical "Best Restaurants" article
#' per city (refreshed periodically). Each venue gets an
#' `<h2 class="...LocationListItemTitle...">` heading followed by a
#' paragraph of editorial prose, and the same article embeds an
#' inline `Restaurant` JSON-LD block per venue with
#' `GeoCoordinates` (latitude / longitude). No detail page fetch is
#' required.
#'
#' The JSON-LD blocks don't include `address` or `servesCuisine`, so
#' we infer cuisine from the editorial description via the shared
#' `prose_to_cuisine()` helper and leave address blank for the
#' geocoder to resolve (or, in practice, ignore - we already have
#' coordinates).
#'
#' @param city Character. Lowercase city slug. Currently supported:
#'   `"honolulu"`. Default `"honolulu"`.
#' @param extra_guides Character vector. Extra Thrillist article URLs
#'   to fetch alongside the city's default best-restaurants page.
#'   Default `character()`.
#' @param use_cache Logical. Cache responses for 24h via the package's
#'   internal `cached_fetch()` helper. Default `FALSE`.
#'
#' @return A tibble with the standard scraper schema.
#' @export
scrape_thrillist <- function(city = "honolulu",
                             extra_guides = character(),
                             use_cache = FALSE) {
  city <- validate_city_source(city, "thrillist")
  cli::cli_h1("Scraping Thrillist: {city}")

  guides <- unique(c(thrillist_default_guides(city), extra_guides))
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
    thrillist_parse_guide(html_str, url)
  })
  results <- purrr::compact(results)
  if (length(results) == 0) {
    cli::cli_abort("No data scraped from any Thrillist guide.")
  }

  combined <- dplyr::bind_rows(results) |>
    dplyr::distinct(.data$name, .keep_all = TRUE)
  cli::cli_alert_success("Found {nrow(combined)} venue{?s}")
  combined
}


#' Default Thrillist article URLs per city
#' @noRd
thrillist_default_guides <- function(city) {
  switch(city,
    honolulu = c(
      "https://www.thrillist.com/eat/honolulu/best-restaurants-honolulu"
    ),
    cli::cli_abort("No default Thrillist guides for {.val {city}}")
  )
}


#' Parse a Thrillist article into a tibble of venue rows
#'
#' Strategy:
#'   1. Pull the `Restaurant` JSON-LD blocks. Each block embeds a
#'      preceding `GeoCoordinates` block, the venue name, and a link
#'      back to Thrillist's venue page. The same venue appears
#'      multiple times in the page payload (carousel + slot
#'      schema-org) so we dedup on name.
#'   2. For each unique venue, locate the matching
#'      `<h2 ...LocationListItemTitle...>NAME</h2>` heading and grab
#'      the following few paragraphs as the description.
#'   3. Infer cuisine from the prose via [prose_to_cuisine()] - the
#'      JSON-LD doesn't carry `servesCuisine` so this is the
#'      strongest signal we have.
#' @noRd
thrillist_parse_guide <- function(html_str, url) {
  # Restaurant JSON-LD: ...,"@type":"Restaurant","name":"NAME","url":"VENUE_URL"...
  # Preceded by GeoCoordinates ~50-100 chars upstream. Regex captures
  # lat/lng/name/url in one pass.
  pattern <- paste0(
    '"@type":"GeoCoordinates",',
    '"latitude":"(-?[0-9.]+)","longitude":"(-?[0-9.]+)"\\}',
    '[^{]*?',
    '"@type":"Restaurant","name":"([^"]+)","url":"([^"]+)"'
  )
  m <- stringr::str_match_all(html_str, pattern)[[1]]
  if (nrow(m) == 0) return(NULL)

  # Dedup on venue name - Thrillist embeds each venue's JSON-LD three
  # times (carousel, slot, page schema).
  dedup_idx <- !duplicated(m[, 4])
  m <- m[dedup_idx, , drop = FALSE]

  rows <- lapply(seq_len(nrow(m)), function(i) {
    raw_name <- m[i, 4]
    name <- thrillist_decode(raw_name)
    if (is.na(name) || !nzchar(name)) return(NULL)

    lat <- suppressWarnings(as.numeric(m[i, 2]))
    lng <- suppressWarnings(as.numeric(m[i, 3]))

    description <- thrillist_extract_description(html_str, raw_name)

    tibble::tibble(
      name         = name,
      suburb       = NA_character_,
      address      = NA_character_,
      cuisine      = prose_to_cuisine(description),
      category     = "Restaurant",
      description  = description,
      price_range  = NA_integer_,
      rating       = NA_real_,
      rating_scale = NA_character_,
      latitude     = lat,
      longitude    = lng,
      url          = thrillist_decode(m[i, 5])
    )
  })
  rows <- purrr::compact(rows)
  if (length(rows) == 0) return(NULL)
  dplyr::bind_rows(rows)
}


#' Pull the first paragraph after the venue's H2 heading
#'
#' Heading shape: `<h2 class="...LocationListItemTitle...">NAME</h2>`
#' followed within a few KB by one or more `<p>` blocks. We grab the
#' first paragraph (or two, if short) and squish to plain text.
#' Returns `NA_character_` when no description block is found.
#' @noRd
thrillist_extract_description <- function(html_str, raw_name) {
  # The H2 may HTML-entity-encode characters (e.g. ' as &#x27;) but the
  # JSON-LD name uses raw forms. Try both.
  name_in_h2 <- thrillist_html_entity_encode(raw_name)
  anchor_re <- paste0(
    "<h2[^>]*LocationListItemTitle[^>]*>\\s*",
    stringr::str_replace_all(name_in_h2, "([\\.\\+\\*\\?\\[\\^\\]\\$\\(\\)\\{\\}\\|\\\\])", "\\\\\\1"),
    "\\s*</h2>"
  )
  pos <- stringr::str_locate(html_str, anchor_re)[1, "end"]
  if (is.na(pos)) {
    # Fall back to looking for the raw (unencoded) form
    raw_re <- paste0(
      "<h2[^>]*LocationListItemTitle[^>]*>\\s*",
      stringr::str_replace_all(raw_name, "([\\.\\+\\*\\?\\[\\^\\]\\$\\(\\)\\{\\}\\|\\\\])", "\\\\\\1"),
      "\\s*</h2>"
    )
    pos <- stringr::str_locate(html_str, raw_re)[1, "end"]
    if (is.na(pos)) return(NA_character_)
  }

  block <- substr(html_str, pos, min(nchar(html_str), pos + 4000L))
  para_match <- stringr::str_match(block, "<p[^>]*>([\\s\\S]*?)</p>")
  if (is.na(para_match[1, 2])) return(NA_character_)

  text <- para_match[1, 2] |>
    stringr::str_replace_all("<[^>]+>", "") |>
    thrillist_decode() |>
    stringr::str_squish()
  if (!nzchar(text)) return(NA_character_)
  if (nchar(text) > 500) text <- paste0(substr(text, 1, 497), "...")
  text
}


#' Re-encode the few HTML entities Thrillist uses when emitting H2
#' headings, so a JSON-LD name like "Mama Kim's" can be matched
#' against the H2-rendered form "Mama Kim&#x27;s".
#' @noRd
thrillist_html_entity_encode <- function(text) {
  if (is.na(text)) return(text)
  text |>
    gsub("'", "&#x27;", x = _, fixed = TRUE) |>
    gsub("&", "&amp;",  x = _, fixed = TRUE)
}


#' Decode Thrillist JSON-LD name escapes
#'
#' Thrillist JSON embeds curly quotes and other Unicode directly,
#' so most names pass through unchanged. Just normalise the few HTML
#' entities that occasionally slip into the JSON via WordPress.
#' @noRd
thrillist_decode <- function(x) {
  if (is.na(x)) return(x)
  x <- tryCatch(
    stringi::stri_unescape_unicode(x),
    error = function(e) x
  )
  decode_html_entities(x)
}
