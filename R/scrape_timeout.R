#' Scrape Time Out best restaurants list
#'
#' Fetches the Time Out best restaurants list for a given city.
#' Uses static HTML parsing via rvest — no JavaScript rendering needed.
#'
#' @param city Character. One of "sydney", "melbourne". Default `"sydney"`.
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

  cli::cli_alert_success("Found {nrow(result)} venue{?s}")
  result
}

#' Build Time Out URL for a city
#' @noRd
timeout_url <- function(city) {
  # Sydney/Melbourne use "the-best-restaurants-in-{city}"
  # Brisbane/Adelaide use "best-restaurants-in-{city}"
  urls <- c(
    sydney    = "https://www.timeout.com/sydney/restaurants/the-best-restaurants-in-sydney",
    melbourne = "https://www.timeout.com/melbourne/restaurants/the-best-restaurants-in-melbourne"
  )
  urls[[city]]
}

#' Parse a single Time Out restaurant card
#' @noRd
timeout_parse_card <- function(card, city) {
  # Name from h3
  name <- rvest::html_element(card, "h3") |>
    rvest::html_text2()

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
