#' Scrape Gourmet Traveller restaurant guide
#'
#' Fetches the Gourmet Traveller best restaurants list for a given city.
#' Uses static HTML parsing via rvest — no JavaScript rendering needed.
#'
#' @param city Character. One of "sydney", "melbourne". Default `"sydney"`.
#'
#' @return A tibble with columns: name, suburb, address, cuisine, category,
#'   description, price_range, rating, rating_scale, latitude, longitude, url.
#' @export
scrape_gourmet_traveller <- function(city = "sydney", use_cache = FALSE) {
  city <- validate_city_source(city, "gourmet_traveller")
  cli::cli_h1("Scraping Gourmet Traveller: {city}")

  url <- gt_url(city)
  cli::cli_alert_info("Fetching {.url {url}}")

  page <- cached_fetch(url, use_cache = use_cache) |>
    rvest::read_html()

  # Two WordPress variants: listicle plugin or Gutenberg group blocks

  items <- rvest::html_elements(page, "div.wp-block-xwp-listicle-item")
  if (length(items) == 0) {
    items <- rvest::html_elements(page, "div.listicle-block")
  }

  if (length(items) == 0) {
    cli::cli_abort("Could not find restaurant items on Gourmet Traveller page.")
  }

  result <- purrr::map(items, function(item) {
    gt_parse_item(item, city)
  }) |>
    dplyr::bind_rows()

  n_parsed <- sum(!is.na(result$name))
  n_failed <- nrow(result) - n_parsed
  if (n_failed > 0) {
    cli::cli_warn("{n_failed}/{nrow(result)} item{?s} failed to parse a venue name")
  }

  cli::cli_alert_success("Found {nrow(result)} venue{?s}")
  result
}

#' URL lookup for Gourmet Traveller city guides
#' @noRd
gt_url <- function(city) {
  # WordPress post IDs are required in the URL
  urls <- c(
    sydney    = "https://www.gourmettraveller.com.au/dining-out/restaurant-guide/best-restaurants-sydney-4475/",
    melbourne = "https://www.gourmettraveller.com.au/dining-out/restaurant-guide/best-restaurants-melbourne-4478/"
  )
  urls[[city]]
}

#' Parse a single Gourmet Traveller restaurant item
#' @noRd
gt_parse_item <- function(item, city) {
  # Name from h2 > a linking to restaurant-reviews
  name <- rvest::html_element(item, "h2 a[href*='restaurant-reviews']") |>
    rvest::html_text2()
  if (is.na(name)) {
    name <- rvest::html_element(item, "h2") |>
      rvest::html_text2()
  }

  # Metadata buttons: expected [price, cuisine, address] in that order
  buttons <- rvest::html_elements(
    item, "div.wp-block-button.is-style-tertiary a.wp-block-button__link"
  ) |>
    rvest::html_text2()

  if (length(buttons) != 3 && length(buttons) > 0) {
    cli::cli_warn(
      "Expected 3 metadata buttons for {.val {name %||% 'unknown'}}, got {length(buttons)}"
    )
  }

  price_str <- buttons[1] %||% NA_character_
  cuisine   <- buttons[2] %||% NA_character_
  address   <- buttons[3] %||% NA_character_

  # Convert dollar signs to integer (1-5)
  price_range <- if (!is.na(price_str) && grepl("^\\$+$", price_str)) {
    nchar(price_str)
  } else {
    NA_integer_
  }

  # Extract suburb from address (last part after comma)
  suburb <- if (!is.na(address)) {
    parts <- stringr::str_split(address, ",\\s*")[[1]]
    stringr::str_squish(parts[length(parts)])
  } else {
    NA_character_
  }

  # Description — try listicle content first, then generic <p>
  desc <- rvest::html_element(item, ".listicle-item__content > p") |>
    rvest::html_text2()
  if (is.na(desc)) {
    all_p <- rvest::html_elements(item, "p") |>
      rvest::html_text2()
    # Skip subtitle/GT tip paragraphs
    all_p <- all_p[!grepl("^GT", all_p, ignore.case = TRUE)]
    desc <- all_p[1] %||% NA_character_
  }

  # Review URL
  review_href <- rvest::html_element(item, "a.wp-block-express-button__link") |>
    rvest::html_attr("href")
  review_url <- if (!is.na(review_href)) {
    if (grepl("^https?://", review_href)) review_href
    else paste0("https://www.gourmettraveller.com.au", review_href)
  } else {
    NA_character_
  }

  tibble::tibble(
    name         = name %||% NA_character_,
    suburb       = suburb,
    address      = address %||% NA_character_,
    cuisine      = stringr::str_to_title(cuisine %||% NA_character_),
    category     = "Restaurant",
    description  = desc,
    price_range  = as.integer(price_range),
    rating       = NA_real_,
    rating_scale = NA_character_,
    latitude     = NA_real_,
    longitude    = NA_real_,
    url          = review_url
  )
}
