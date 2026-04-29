#' Scrape The Infatuation's editorial guides
#'
#' The Infatuation publishes city-by-city editorial guides ("21
#' Restaurants To Make You Fall In Love With SF Again", "Best Italian
#' Restaurants in SF", etc.). Each guide page embeds a structured
#' JSON-LD ItemList with the full venue details (name, address,
#' coordinates, cuisine, price, description), so a single HTTP fetch
#' per guide is enough — no per-venue detail call needed.
#'
#' By default we pull the flagship "best of" list per city. Pass a
#' character vector of additional guide slugs (e.g. `"best-italian-
#' restaurants-san-francisco"`) via `extra_guides` to broaden
#' coverage across cuisines / occasions / categories.
#'
#' @param city Character. Lowercase city slug. Currently supported:
#'   `"san-francisco"`. Default `"san-francisco"`.
#' @param extra_guides Character vector. Additional guide slugs to
#'   fetch on top of the flagship list. Default `character()`.
#' @param use_cache Logical. If `TRUE`, cache each guide page locally
#'   (24-hour TTL via `cached_fetch()`). Default `FALSE`.
#'
#' @return A tibble with columns: name, suburb, address, cuisine,
#'   category, description, price_range, rating, rating_scale, latitude,
#'   longitude, url.
#' @export
scrape_infatuation <- function(city = "san-francisco",
                               extra_guides = character(),
                               use_cache = FALSE) {
  city <- validate_city_source(city, "infatuation")
  cli::cli_h1("Scraping The Infatuation: {city}")

  guides <- unique(c(infatuation_default_guides(city), extra_guides))
  cli::cli_alert_info("Fetching {length(guides)} guide{?s}")

  results <- purrr::map(guides, function(slug) {
    url <- paste0("https://www.theinfatuation.com/", city, "/guides/", slug)
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
    infatuation_parse_guide(html_str)
  })
  results <- purrr::compact(results)
  if (length(results) == 0) {
    cli::cli_abort("No data scraped from any Infatuation guide.")
  }

  combined <- dplyr::bind_rows(results) |>
    dplyr::distinct(.data$name, .data$suburb, .keep_all = TRUE)
  cli::cli_alert_success("Found {nrow(combined)} venue{?s}")
  combined
}


#' Default guide slugs per city
#'
#' For SF, hits the flagship restaurants list plus a handful of
#' category-specific guides covering bars, brunch, coffee/cafes and a
#' few cuisines so the combined output spans restaurants + bars +
#' cafes. Failures (404s) for individual slugs are logged but don't
#' abort the overall scrape.
#' @noRd
infatuation_default_guides <- function(city) {
  switch(city,
    `san-francisco` = c(
      "restaurants-that-remind-us-why-we-love-sf",
      "great-sf-restaurants-for-dining-solo",
      "best-italian-restaurants-san-francisco",
      "best-pizza-san-francisco",
      "best-ramen-san-francisco",
      "best-pho-sf",
      "best-noodle-soup-san-francisco",
      "the-best-pasta-in-san-francisco",
      "best-croissants-sf",
      "best-matcha-san-francisco",
      "best-cha-chaan-teng-sf"
    ),
    cli::cli_abort("No default Infatuation guides for {.val {city}}")
  )
}


#' Parse all venues out of an Infatuation guide page's JSON-LD
#' @noRd
infatuation_parse_guide <- function(html_str) {
  page <- rvest::read_html(html_str)
  blocks <- rvest::html_elements(page, "script[type='application/ld+json']") |>
    rvest::html_text()

  rows <- list()
  for (b in blocks) {
    parsed <- tryCatch(
      jsonlite::fromJSON(b, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) next
    items <- if (identical(parsed$`@type`, "ItemList")) {
      parsed$itemListElement
    } else {
      list()
    }
    for (entry in items) {
      inner <- entry$item %||% entry
      if (is.null(inner) || !identical(inner$`@type`, "Restaurant")) next
      rows[[length(rows) + 1]] <- infatuation_row_from_ld(inner)
    }
  }

  if (length(rows) == 0) {
    return(NULL)
  }
  dplyr::bind_rows(purrr::compact(rows))
}


#' Build a single restaurant tibble row from a JSON-LD Restaurant entity
#' @noRd
infatuation_row_from_ld <- function(item) {
  addr     <- item$address %||% list()
  geo      <- item$geo %||% list()
  cuisines <- item$servesCuisine
  cuisine  <- if (length(cuisines) > 0) {
    paste(unlist(cuisines), collapse = ", ")
  } else {
    NA_character_
  }

  # Infatuation's addressLocality is always the city (e.g. "San Francisco");
  # the actual neighborhood lives in their custom HTML, not the JSON-LD,
  # so suburb is left as the city for now.
  suburb <- addr$addressLocality %||% NA_character_

  street <- addr$streetAddress %||% NA_character_
  full_address <- if (!is.na(street)) {
    paste(c(street, addr$addressLocality, addr$addressRegion, addr$postalCode),
          collapse = ", ") |>
      gsub(", NA", "", x = _, fixed = TRUE)
  } else {
    addr$name %||% NA_character_
  }

  price_str <- item$priceRange %||% NA_character_
  price_range <- if (!is.na(price_str)) nchar(gsub("[^$]", "", price_str)) else NA_integer_

  tibble::tibble(
    name         = decode_html_entities(item$name %||% NA_character_),
    suburb       = decode_html_entities(suburb),
    address      = decode_html_entities(full_address),
    cuisine      = decode_html_entities(cuisine),
    category     = "Restaurant",
    description  = decode_html_entities(item$description %||% NA_character_),
    price_range  = as.integer(price_range),
    rating       = NA_real_,
    rating_scale = NA_character_,
    # Guard against NULL geo fields - as.numeric(NULL) returns
    # numeric(0), which would recycle the whole row out of existence
    # under tibble's column-length rule.
    latitude     = suppressWarnings(as.numeric(geo$latitude %||% NA_real_)),
    longitude    = suppressWarnings(as.numeric(geo$longitude %||% NA_real_)),
    url          = item$url %||% NA_character_
  )
}
