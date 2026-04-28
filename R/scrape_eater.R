#' Scrape Eater's "Essential X" + map guides
#'
#' Eater publishes city-by-city map guides ("The 38 Best Restaurants in
#' San Francisco", "Heatmap of New Restaurants", "Best Bars", etc.).
#' Each map page embeds the venue payload as inline JSON inside the
#' article HTML (one block per venue with `location.latitude`,
#' `location.longitude`, `name`, `venue.address`). We pull those out
#' with a couple of regexes — no per-venue detail fetch needed.
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
    eater_parse_guide(html_str)
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
      "best-steakhouses-san-francisco"
    ),
    cli::cli_abort("No default Eater guides for {.val {city}}")
  )
}


#' Parse all venues out of an Eater map page's inline JSON
#' @noRd
eater_parse_guide <- function(html_str) {
  # Each venue's data is interleaved in the article HTML. The pattern is
  #   "location":{"latitude":X,"longitude":Y},"name":"NAME"
  # followed (within ~3 KB) by
  #   "venue":{...,"address":"ADDR"...}
  loc_re <- '"location":\\{"latitude":(-?[0-9.]+),"longitude":(-?[0-9.]+)\\},"name":"([^"]+)"'
  m <- stringr::str_match_all(html_str, loc_re)[[1]]
  if (nrow(m) == 0) return(NULL)

  starts <- stringr::str_locate_all(html_str, loc_re)[[1]][, "start"]

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

    tibble::tibble(
      name         = name,
      suburb       = suburb,
      address      = addr,
      cuisine      = NA_character_,
      category     = "Restaurant",
      description  = NA_character_,
      price_range  = NA_integer_,
      rating       = NA_real_,
      rating_scale = NA_character_,
      latitude     = lat,
      longitude    = lng,
      url          = NA_character_
    )
  })

  dplyr::bind_rows(rows)
}


#' Extract the suburb (city/neighborhood) from an Eater address string
#'
#' Eater addresses look like "2700 Jones St, San Francisco, CA, 94133, US".
#' We return the second-to-last comma section before the state — which
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


#' Decode the Eater-flavoured escapes (Unicode + backslash JSON)
#' @noRd
eater_unescape <- function(x) {
  if (is.na(x)) return(x)
  # Replace \uXXXX escapes with the literal char
  x <- stringi::stri_unescape_unicode(x)
  decode_html_entities(x)
}
