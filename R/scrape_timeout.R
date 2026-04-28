#' Scrape Time Out best restaurants list
#'
#' Fetches the Time Out best restaurants list for a given city.
#' Uses static HTML parsing via rvest — no JavaScript rendering needed.
#'
#' @param city Character. One of "sydney", "melbourne". Default `"sydney"`.
#' @param use_cache Logical. Use cached HTTP responses if available. Default `FALSE`.
#'
#' @return A tibble with columns: name, suburb, address, cuisine, category,
#'   description, price_range, rating, rating_scale, latitude, longitude, url.
#' @export
scrape_timeout <- function(city = "sydney", use_cache = FALSE) {
  city <- validate_city_source(city, "timeout")
  cli::cli_h1("Scraping Time Out: {city}")

  url <- timeout_url(city)
  cli::cli_alert_info("Fetching {.url {url}}")

  page <- cached_fetch(url, use_cache = use_cache) |>
    rvest::read_html()

  # Select restaurant articles, excluding promotional recirc cards
  cards <- rvest::html_elements(page, "article.tile:not(.list-card-recirc-inline)")

  if (length(cards) == 0) {
    cli::cli_abort("Could not find restaurant cards on Time Out page.")
  }

  result <- purrr::map(cards, function(card) {
    timeout_parse_card(card, city)
  }) |>
    dplyr::bind_rows()

  # Remove rows where name is NA (bad parse)
  n_before <- nrow(result)
  result <- dplyr::filter(result, !is.na(.data$name))
  n_dropped <- n_before - nrow(result)
  if (n_dropped > 0) {
    cli::cli_warn("{n_dropped}/{n_before} card{?s} failed to parse (no venue name)")
  }

  cli::cli_alert_success("Found {nrow(result)} venue{?s} from listing")

  # Phase 2: Fetch detail pages to fill missing cuisine, description, coords
  n_with_url <- sum(!is.na(result$url))
  if (n_with_url > 0) {
    cli::cli_alert_info("Fetching {n_with_url} detail page{?s} for extra data...")
    for (i in seq_len(nrow(result))) {
      if (is.na(result$url[i])) next
      Sys.sleep(RATE_LIMIT_SECS)
      detail <- timeout_fetch_detail(result$url[i], use_cache = use_cache)
      if (is.null(detail)) next

      # Fill missing fields from detail page
      if (is.na(result$cuisine[i]) && !is.na(detail$cuisine)) {
        result$cuisine[i] <- detail$cuisine
      }
      if ((is.na(result$description[i]) || result$description[i] == "") &&
          !is.na(detail$description)) {
        result$description[i] <- detail$description
      }
      if (is.na(result$suburb[i]) && !is.na(detail$suburb)) {
        result$suburb[i] <- detail$suburb
      }
      if (is.na(result$address[i]) && !is.na(detail$address)) {
        result$address[i] <- detail$address
      }
      if (is.na(result$latitude[i]) && !is.na(detail$latitude)) {
        result$latitude[i] <- detail$latitude
        result$longitude[i] <- detail$longitude
      }
      if (is.na(result$price_range[i]) && !is.na(detail$price_range)) {
        result$price_range[i] <- detail$price_range
      }

      if (i %% 20 == 0) cli::cli_alert_info("  ...{i}/{nrow(result)}")
    }
    cli::cli_alert_success("Detail pages enriched venue data")
  }

  result
}

#' Build Time Out URL for a city
#' @noRd
timeout_url <- function(city) {
  # Sydney/Melbourne use "the-best-restaurants-in-{city}"
  # Brisbane/Adelaide use "best-restaurants-in-{city}"
  urls <- c(
    sydney          = "https://www.timeout.com/sydney/restaurants/the-best-restaurants-in-sydney",
    melbourne       = "https://www.timeout.com/melbourne/restaurants/the-best-restaurants-in-melbourne",
    `san-francisco` = "https://www.timeout.com/san-francisco/restaurants/the-best-restaurants-in-san-francisco"
  )
  urls[[city]]
}

#' Parse a single Time Out restaurant card
#' @noRd
timeout_parse_card <- function(card, city) {
  # Name from h3. Strip leading rank prefix (Time Out SF formats their
  # tiles as "1. Copra", "2. 7 Adams" etc.; Sydney/Melbourne don't).
  name <- rvest::html_element(card, "h3") |>
    rvest::html_text2() |>
    stringr::str_remove("^\\s*\\d+\\.\\s*")

  # URL from tile link
  href <- rvest::html_element(card, "a[data-testid='tile-link_testID']") |>
    rvest::html_attr("href")
  venue_url <- if (!is.na(href)) {
    paste0("https://www.timeout.com", href)
  } else {
    NA_character_
  }

  # Price from data-testid price rating
  price_text <- rvest::html_element(card, "[data-testid='price-rating_testID']") |>
    rvest::html_text2()
  price_range <- if (!is.na(price_text)) {
    as.integer(stringr::str_extract(price_text, "\\d+"))
  } else {
    NA_integer_
  }

  # Star rating (1-5) from data-testid
  rating_text <- rvest::html_element(card, "[data-testid='star-rating_testID']") |>
    rvest::html_text2()
  rating <- if (!is.na(rating_text)) {
    as.double(stringr::str_extract(rating_text, "\\d+"))
  } else {
    NA_real_
  }

  # Plain text tags (cuisine / suburb — not structurally distinguishable)
  plain_tags <- rvest::html_elements(card, "section[data-testid='tags_testID'] li") |>
    purrr::keep(function(li) {
      # Only plain tags (no data-testid children like star-rating, price-rating)
      length(rvest::html_elements(li, "[data-testid]")) == 0
    }) |>
    rvest::html_text2() |>
    stringr::str_squish()

  # Heuristic: last plain tag is often the suburb
  suburb <- if (length(plain_tags) > 0) plain_tags[length(plain_tags)] else NA_character_
  cuisine <- if (length(plain_tags) > 1) plain_tags[1] else NA_character_

  # Summary text block
  summary <- rvest::html_element(card, "div[class*='_summary_']") |>
    rvest::html_text2()

  # Extract address from summary (text after "Address:")
  # Stop before "Expect to pay" or end of line
  address <- if (!is.na(summary)) {
    stringr::str_extract(summary, "(?i)(?<=Address:\\s?)(.+?)(?=Expect to pay|Price:|\\n|$)") |>
      stringr::str_squish()
  } else {
    NA_character_
  }

  # Extract "What is it?" as the description
  description <- if (!is.na(summary)) {
    stringr::str_extract(summary, "(?i)(?<=What is it\\?\\s?)(.+?)(?=Why we love it|Why go\\?|Time Out tip|Address:|$)") |>
      stringr::str_squish()
  } else {
    NA_character_
  }

  tibble::tibble(
    name         = name %||% NA_character_,
    suburb       = suburb,
    address      = address %||% NA_character_,
    cuisine      = cuisine,
    category     = "Restaurant",
    description  = description,
    price_range  = as.integer(price_range),
    rating       = rating,
    rating_scale = if (!is.na(rating)) "timeout_stars_5" else NA_character_,
    latitude     = NA_real_,
    longitude    = NA_real_,
    url          = venue_url
  )
}

#' Fetch and parse a Time Out venue detail page for extra metadata
#'
#' Extracts JSON-LD Restaurant schema and digitalData coordinates from
#' the venue's detail page.
#'
#' @param url Character. Full URL of the venue detail page.
#' @param use_cache Logical. Use HTTP cache.
#' @return A named list with fields: cuisine, description, suburb, address,
#'   latitude, longitude, price_range. All may be NA.
#' @noRd
timeout_fetch_detail <- function(url, use_cache = FALSE) {
  html <- tryCatch(
    cached_fetch(url, use_cache = use_cache),
    error = function(e) NULL
  )
  if (is.null(html)) return(NULL)

  page <- rvest::read_html(html)

  # Try JSON-LD first
  json_ld <- timeout_extract_jsonld(page)

  # Try digitalData for coordinates
  coords <- timeout_extract_digital_data(html)

  list(
    cuisine     = json_ld$cuisine,
    description = json_ld$description,
    suburb      = json_ld$suburb,
    address     = json_ld$address,
    latitude    = coords$latitude %||% json_ld$latitude,
    longitude   = coords$longitude %||% json_ld$longitude,
    price_range = json_ld$price_range
  )
}

#' Extract venue data from JSON-LD Restaurant schema
#' @noRd
timeout_extract_jsonld <- function(page) {
  defaults <- list(
    cuisine = NA_character_, description = NA_character_,
    suburb = NA_character_, address = NA_character_,
    latitude = NA_real_, longitude = NA_real_,
    price_range = NA_integer_
  )

  scripts <- rvest::html_elements(page, "script[type='application/ld+json']")
  for (script in scripts) {
    json_text <- rvest::html_text(script)
    parsed <- tryCatch(jsonlite::fromJSON(json_text, simplifyVector = FALSE),
                       error = function(e) NULL)
    if (is.null(parsed)) next

    # Handle both single objects and arrays
    items <- if (is.null(parsed[["@type"]])) parsed else list(parsed)
    for (item in items) {
      if (!identical(item[["@type"]], "Restaurant")) next

      # Cuisine
      cuisine <- item[["servesCuisine"]]
      if (is.list(cuisine)) cuisine <- paste(unlist(cuisine), collapse = ", ")

      # Address
      addr <- item[["address"]]
      suburb <- addr[["addressLocality"]]
      street <- addr[["streetAddress"]]
      postcode <- addr[["postalCode"]]
      full_address <- if (!is.null(street)) {
        parts <- c(street, suburb, postcode)
        paste(parts[!is.null(parts)], collapse = ", ")
      } else {
        NA_character_
      }

      # Description from review text
      review <- item[["review"]]
      desc <- if (is.list(review)) {
        review_text <- review[["reviewBody"]] %||% review[["text"]]
        if (!is.null(review_text) && nchar(review_text) > 0) {
          # Truncate very long reviews to first 500 chars
          if (nchar(review_text) > 500) {
            paste0(substr(review_text, 1, 497), "...")
          } else {
            review_text
          }
        }
      }

      # Price range: convert "$" / "$$" / "$$$" / "$$$$" to integer
      price_text <- item[["priceRange"]]
      price_int <- if (!is.null(price_text) && !is.na(price_text)) {
        n <- nchar(gsub("[^$]", "", price_text))
        if (is.na(n) || n == 0) NA_integer_ else as.integer(n)
      } else {
        NA_integer_
      }

      return(list(
        cuisine     = cuisine %||% NA_character_,
        description = desc %||% NA_character_,
        suburb      = suburb %||% NA_character_,
        address     = full_address %||% NA_character_,
        latitude    = NA_real_,
        longitude   = NA_real_,
        price_range = as.integer(price_int %||% NA_integer_)
      ))
    }
  }

  defaults
}

#' Extract coordinates from window.digitalData JavaScript object
#' @noRd
timeout_extract_digital_data <- function(html) {
  # Look for geo coordinates in digitalData
  lat <- stringr::str_match(html, '"latitude"\\s*:\\s*(-?[0-9]+\\.?[0-9]*)')[, 2]
  lng <- stringr::str_match(html, '"longitude"\\s*:\\s*(-?[0-9]+\\.?[0-9]*)')[, 2]

  list(
    latitude  = if (!is.na(lat[1])) as.double(lat[1]) else NA_real_,
    longitude = if (!is.na(lng[1])) as.double(lng[1]) else NA_real_
  )
}
