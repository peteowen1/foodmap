#' Scrape Conde Nast Traveler "Best Restaurants" gallery articles
#'
#' CNT publishes city-by-city editorial gallery articles ("The Best
#' Restaurants in San Francisco" etc.). The pages are dynamic but the
#' venue names are reliably exposed as `<h3>` tags inside each gallery
#' chunk, with a paragraph-of-prose underneath. Static HTML doesn't
#' include addresses or coordinates, so we lean on the geocoder to
#' resolve location from name + city.
#'
#' @param city Character. Currently `"san-francisco"`. Default
#'   `"san-francisco"`.
#' @param extra_guides Character vector of additional CNT article URLs.
#' @param use_cache Logical. Cache responses for 24h. Default `FALSE`.
#'
#' @return A tibble with the standard scraper schema.
#' @export
scrape_cn_traveler <- function(city = "san-francisco",
                                extra_guides = character(),
                                use_cache = FALSE) {
  city <- validate_city_source(city, "cn_traveler")
  cli::cli_h1("Scraping Conde Nast Traveler: {city}")

  guides <- unique(c(cnt_default_guides(city), extra_guides))
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
    cnt_parse_guide(html_str, city)
  })
  results <- purrr::compact(results)
  if (length(results) == 0) {
    cli::cli_abort("No data scraped from any CNT guide.")
  }

  combined <- dplyr::bind_rows(results) |>
    dplyr::distinct(.data$name, .data$suburb, .keep_all = TRUE)
  cli::cli_alert_success("Found {nrow(combined)} venue{?s}")
  combined
}


#' Default CNT gallery URLs per city
#' @noRd
cnt_default_guides <- function(city) {
  switch(city,
    `san-francisco` = c(
      "https://www.cntraveler.com/gallery/best-restaurants-in-san-francisco"
    ),
    cli::cli_abort("No default CNT guides for {.val {city}}")
  )
}


#' Parse a CNT gallery article into venue rows
#'
#' CNT emits invalid HTML where each venue name is wrapped as
#' `<h3 class="UnifiedVenueCardName-..."><p>Name</p></h3>` - browsers
#' (and rvest) auto-close the h3 before the inner `<p>` per HTML5
#' rules, leaving the h3's text content empty. So we extract by
#' regex against the raw HTML, anchored on the venue-card class.
#' Suburb defaults to the city (CNT doesn't include neighborhood
#' data in the static HTML); the geocoder resolves the rest.
#' @noRd
cnt_parse_guide <- function(html_str, city) {
  # Names live in <h3 class="...UnifiedVenueCardName..."><p>NAME</p></h3>
  name_pattern <- "<h3[^>]*UnifiedVenueCardName[^>]*>\\s*<p>([^<]+)</p>"
  matches <- stringr::str_match_all(html_str, name_pattern)[[1]]
  starts  <- stringr::str_locate_all(html_str, name_pattern)[[1]][, "start"]
  if (nrow(matches) == 0) return(NULL)

  city_pretty <- gsub("-", " ", city) |> tools::toTitleCase()

  # Description text follows each name within a few KB inside a
  # UnifiedProductCardDescriptionWrapper block. Pull the first <p>
  # after the wrapper opens, escape-decode it, and stash for cuisine
  # inference downstream.
  desc_re <- "UnifiedProductCardDescriptionWrapper[^>]*>\\s*<div>\\s*<p>([^<]+)"

  rows <- lapply(seq_len(nrow(matches)), function(i) {
    name <- decode_html_entities(stringr::str_squish(matches[i, 2]))
    if (nchar(name) < 2 || nchar(name) > 100) return(NULL)

    # Look for description in the chunk between this name and the next
    chunk_start <- starts[i]
    chunk_end   <- if (i < nrow(matches)) starts[i + 1] else nchar(html_str)
    chunk       <- substr(html_str, chunk_start, chunk_end)
    desc_match  <- stringr::str_match(chunk, desc_re)
    description <- if (!is.na(desc_match[1, 2])) {
      decode_html_entities(stringr::str_squish(desc_match[1, 2]))
    } else {
      NA_character_
    }

    tibble::tibble(
      name         = name,
      suburb       = city_pretty,
      address      = NA_character_,
      cuisine      = cnt_description_to_cuisine(description),
      category     = "Restaurant",
      description  = description,
      price_range  = NA_integer_,
      rating       = NA_real_,
      rating_scale = NA_character_,
      latitude     = NA_real_,
      longitude    = NA_real_,
      url          = NA_character_
    )
  })
  rows <- purrr::compact(rows)
  if (length(rows) == 0) return(NULL)
  dplyr::bind_rows(rows)
}


#' Infer cuisine from a CN Traveler editorial description
#'
#' Thin wrapper around the shared `prose_to_cuisine()` helper -
#' keeping the named function around for backwards-compatibility with
#' anything that imports it, but the matching rules now live in one
#' place across scrapers.
#' @noRd
cnt_description_to_cuisine <- function(description) {
  prose_to_cuisine(description)
}
