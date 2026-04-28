#' Scrape Concrete Playground's "best of" restaurant guide
#'
#' Concrete Playground runs a 1400-venue restaurant directory plus a
#' tightly-curated editorial "best of" article. We pull the curated
#' list rather than the directory because the directory's user-rating
#' filter is client-side only - it doesn't actually constrain the page
#' response, so scraping their high-confidence picks means scraping the
#' editorial guide.
#'
#' Each venue card embeds JSON-LD with name, suburb (addressLocality),
#' street address, lat/lng, served cuisines and a description, so we
#' get useable data without a per-venue detail fetch.
#'
#' @param city Character. One of `"sydney"`, `"melbourne"`. Default
#'   `"sydney"`.
#' @param use_cache Logical. If `TRUE`, cache the page response locally
#'   (24-hour TTL via `cached_fetch()`). Default `FALSE`.
#'
#' @return A tibble with columns: name, suburb, address, cuisine,
#'   category, description, price_range, rating, rating_scale, latitude,
#'   longitude, url.
#' @export
scrape_concrete_playground <- function(city = "sydney", use_cache = FALSE) {
  city <- validate_city_source(city, "concrete_playground")
  cli::cli_h1("Scraping Concrete Playground: {city}")

  base_url <- cp_directory_url(city)
  cli::cli_alert_info("Fetching {.url {base_url}}")
  html_str <- cached_fetch(base_url, use_cache = use_cache)
  page1 <- rvest::read_html(html_str)

  # Detect total result count and page count from the rendered HTML so
  # we know how far to paginate. Falls back to a single page if the
  # count can't be parsed.
  total <- cp_total_results(page1)
  per_page <- length(rvest::html_elements(page1, "li[data-latitude]"))

  pages <- list(page1)
  if (!is.na(total) && per_page > 0 && total > per_page) {
    n_pages <- ceiling(total / per_page)
    cli::cli_alert_info("{total} CP Picks total - fetching {n_pages - 1} more page{?s}")
    for (p in seq.int(2, n_pages)) {
      Sys.sleep(RATE_LIMIT_SECS)
      url_p <- paste0(base_url, "&paged=", p)
      html_p <- tryCatch(
        cached_fetch(url_p, use_cache = use_cache),
        error = function(e) NULL
      )
      if (is.null(html_p)) next
      pages[[length(pages) + 1]] <- rvest::read_html(html_p)
    }
  }

  cards <- unlist(
    lapply(pages, function(p) rvest::html_elements(p, "li[data-latitude]")),
    recursive = FALSE, use.names = FALSE
  )
  cli::cli_alert_info("Parsing {length(cards)} venue card{?s}")
  if (length(cards) == 0) {
    cli::cli_abort("No venue cards on Concrete Playground page.")
  }

  raw_results <- purrr::map(cards, cp_parse_card)
  n_failed <- sum(purrr::map_lgl(raw_results, is.null))
  if (n_failed > 0) {
    cli::cli_warn("{n_failed}/{length(cards)} card{?s} failed to parse")
  }
  results <- purrr::compact(raw_results)
  if (length(results) == 0) {
    cli::cli_abort("Could not extract any venue data from Concrete Playground.")
  }

  result <- dplyr::bind_rows(results) |>
    dplyr::distinct(.data$name, .data$suburb, .keep_all = TRUE)
  cli::cli_alert_success("Found {nrow(result)} venue{?s}")
  result
}


#' Build the CP-Pick filtered directory URL for a city
#' @noRd
cp_directory_url <- function(city) {
  base <- switch(city,
    sydney    = "https://concreteplayground.com/sydney/restaurants",
    melbourne = "https://concreteplayground.com/melbourne/restaurants",
    cli::cli_abort("Unknown city for Concrete Playground: {.val {city}}")
  )
  paste0(base, "?features%5B%5D=CP+Pick")
}


#' Find the total CP Pick result count from the rendered HTML
#'
#' The "Show N results" button text contains the count. If we can't
#' find it, return NA so the caller falls back to a single page.
#' @noRd
cp_total_results <- function(page) {
  text <- rvest::html_text2(page)
  m <- stringr::str_extract(text, "Show\\s+(\\d[\\d,]*)\\s+result")
  if (is.na(m)) return(NA_integer_)
  num <- stringr::str_extract(m, "\\d[\\d,]*") |>
    stringr::str_remove_all(",") |>
    as.integer()
  num
}


#' Parse a single Concrete Playground venue card into a tibble row
#'
#' Each card contains a `<code class="data-item">` block with a JSON
#' payload that has every field we need (name, description, lat, lng,
#' address, url). We use that as the canonical source and fall back to
#' the parent `<li>`'s data-latitude attribute only if the JSON is
#' malformed.
#' @noRd
cp_parse_card <- function(card) {
  data_item_text <- rvest::html_element(card, "code.data-item") |>
    rvest::html_text()
  di <- tryCatch(
    jsonlite::fromJSON(data_item_text %||% "{}", simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(di) || is.null(di$name)) return(NULL)

  name <- decode_html_entities(di$name)

  # Address is "<street>, <suburb>" - split on last comma to get suburb
  raw_addr <- decode_html_entities(di$address %||% NA_character_)
  suburb   <- NA_character_
  if (!is.na(raw_addr) && grepl(",", raw_addr)) {
    parts  <- stringr::str_split(raw_addr, ",\\s*")[[1]]
    suburb <- stringr::str_squish(parts[length(parts)])
  }

  description <- di$description %||% NA_character_
  if (!is.na(description)) description <- decode_html_entities(description)

  lat <- suppressWarnings(as.numeric(di$latitude))
  lng <- suppressWarnings(as.numeric(di$longitude))
  if (is.na(lat)) lat <- suppressWarnings(as.numeric(rvest::html_attr(card, "data-latitude")))
  if (is.na(lng)) lng <- suppressWarnings(as.numeric(rvest::html_attr(card, "data-longitude")))

  tibble::tibble(
    name         = name %||% NA_character_,
    suburb       = suburb,
    address      = raw_addr,
    cuisine      = NA_character_,
    category     = "Restaurant",
    description  = description,
    price_range  = NA_integer_,
    rating       = NA_real_,
    rating_scale = NA_character_,
    latitude     = lat,
    longitude    = lng,
    url          = di$url %||% NA_character_
  )
}


# decode_html_entities() lives in utils.R so other scrapers can reuse it.
