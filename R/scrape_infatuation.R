#' Scrape The Infatuation's editorial guides
#'
#' The Infatuation publishes city-by-city editorial guides ("21
#' Restaurants To Make You Fall In Love With SF Again", "Best Italian
#' Restaurants in SF", etc.). Each guide page embeds a structured
#' JSON-LD ItemList with the full venue details (name, address,
#' coordinates, cuisine, price, description), so a single HTTP fetch
#' per guide is enough -- no per-venue detail call needed.
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

  # The Infatuation's URL slug doesn't always match the city slug we
  # use as our dispatcher key. Honolulu's guides live under
  # `/oahu/guides/...` because the publication groups its Hawai'i
  # coverage by island. Resolved here so other call sites stay
  # city-keyed.
  url_city <- infatuation_url_city(city)

  results <- purrr::map(guides, function(slug) {
    url <- paste0("https://www.theinfatuation.com/", url_city, "/guides/", slug)
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
      "best-cha-chaan-teng-sf",
      # Cafe / coffee / bakery / breakfast guides - the Sydneysider's
      # cafe corner. These broaden cuisine coverage so the cafe filter
      # actually has something to show beyond a handful of bakeries.
      "best-coffee-shops-san-francisco",
      "best-breakfast-san-francisco",
      "best-brunch-san-francisco",
      "best-lunch-in-san-francisco",
      "great-savory-croissants-sf",
      "pop-up-bakeries-san-francisco"
    ),
    # Honolulu lives under /oahu/guides/. The Infatuation publishes
    # only two Honolulu guides as of late 2025 (one restaurants, one
    # bars), so the default set is much smaller than SF's. Both pages
    # embed the same JSON-LD ItemList shape that the SF parser already
    # handles, so no code changes are needed beyond URL routing.
    honolulu = c(
      "best-restaurants-oahu-honolulu-waikiki-hawaii",
      "best-bars-honolulu"
    ),
    `new-york` = c(
      "best-restaurants-nyc",
      "best-new-new-york-restaurants-hit-list",
      "best-new-brooklyn-restaurants-hit-list",
      "best-italian-restaurants-nyc",
      "best-brunch-restaurants-nyc",
      "best-burger-nyc",
      "best-cheap-eats-nyc",
      "best-date-night-restaurants-nyc",
      "best-fun-cool-bars-nyc-right-now",
      "best-asian-desserts-nyc",
      "best-restaurants-flatiron",
      "best-restaurants-little-italy-nyc"
    ),
    `los-angeles` = c(
      "best-restaurants-los-angeles",
      "best-new-los-angeles-restaurants-hit-list",
      "best-chinese-food-los-angeles",
      "best-sushi-in-los-angeles",
      "best-mariscos-restaurants-in-la",
      "best-brunch-la-greatest-hits-list",
      "best-restaurants-in-santa-monica",
      "best-la-restaurants-to-eat-at-the-bar",
      "affordable-romantic-restaurants-los-angeles"
    ),
    london = c(
      "best-restaurants-london",
      "best-new-london-restaurants-hit-list",
      "best-bakeries-in-london",
      "best-cheap-eats-london",
      "best-fish-and-chips-london",
      "best-east-london-brunch",
      "best-outdoor-restaurants-london",
      "best-pub-gardens-london",
      "best-drinks-cocktails-london",
      "best-london-coffee-shops-with-outdoor-seating"
    ),
    cli::cli_abort("No default Infatuation guides for {.val {city}}")
  )
}


#' Map our city slug to the path segment The Infatuation uses
#'
#' Most cities are 1:1 (`san-francisco` -> `san-francisco`), but
#' Honolulu lives under the island grouping (`oahu`) on their site.
#' @noRd
infatuation_url_city <- function(city) {
  switch(city,
    `san-francisco` = "san-francisco",
    honolulu        = "oahu",
    `new-york`      = "new-york",
    `los-angeles`   = "los-angeles",
    london          = "london",
    cli::cli_abort("No Infatuation URL mapping for {.val {city}}")
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
