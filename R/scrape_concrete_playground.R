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

  # CP runs three CP-Pick-filtered directories per city: restaurants,
  # bars, cafes. They share the same card markup so we can reuse one
  # parser and just point it at each path. Restaurants are typically
  # ~200-300 picks, bars ~100, cafes ~150 - so the bar/cafe expansion
  # is roughly a 50% lift in CP-sourced venues.
  sections <- c("restaurants", "bars", "cafes")
  per_section <- purrr::map(sections, function(sec) {
    cp_scrape_section(city = city, section = sec, use_cache = use_cache)
  })
  per_section <- purrr::compact(per_section)
  if (length(per_section) == 0) {
    cli::cli_abort("No data scraped from any Concrete Playground section.")
  }

  result <- dplyr::bind_rows(per_section) |>
    # Same venue can be tagged CP-Pick in multiple sections (a bar that
    # also serves food shows up under both restaurants and bars). Keep
    # the first occurrence - sections are processed in order, so the
    # restaurant variant wins where both exist, which usually carries
    # the richer cuisine tags.
    dplyr::distinct(.data$name, .data$suburb, .keep_all = TRUE)
  cli::cli_alert_success("Found {nrow(result)} venue{?s}")
  result
}


#' Scrape a single CP section (restaurants / bars / cafes) for a city
#'
#' Returns NULL on section-level failure (e.g. 404) so the outer loop
#' can keep the other sections.
#' @noRd
cp_scrape_section <- function(city, section, use_cache) {
  base_url <- cp_directory_url(city, section)
  cli::cli_alert_info("Fetching {section} - {.url {base_url}}")
  html_str <- tryCatch(
    cached_fetch(base_url, use_cache = use_cache),
    error = function(e) {
      cli::cli_warn("CP {section} failed: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(html_str)) return(NULL)
  page1 <- rvest::read_html(html_str)

  total <- cp_total_results(page1)
  per_page <- length(rvest::html_elements(page1, "li[data-latitude]"))

  pages <- list(page1)
  if (!is.na(total) && per_page > 0 && total > per_page) {
    n_pages <- ceiling(total / per_page)
    cli::cli_alert_info(
      "  {total} {section} picks - fetching {n_pages - 1} more page{?s}"
    )
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
  if (length(cards) == 0) {
    cli::cli_warn("No venue cards in CP {section} for {city}")
    return(NULL)
  }

  raw <- purrr::map(cards, cp_parse_card)
  n_failed <- sum(purrr::map_lgl(raw, is.null))
  if (n_failed > 0) {
    cli::cli_warn("  {n_failed}/{length(cards)} {section} card{?s} failed to parse")
  }
  rows <- purrr::compact(raw)
  if (length(rows) == 0) return(NULL)

  out <- dplyr::bind_rows(rows)
  # Override the parser-default "Restaurant" category for bars and cafes
  # so the downstream category-mix analysis correctly tallies them. The
  # cp_parse_card function returns the JSON-LD venueType which is usually
  # "Restaurant" even for bars/cafes; the section path is more reliable.
  if (section == "bars" && "category" %in% names(out)) out$category <- "Bar"
  if (section == "cafes" && "category" %in% names(out)) out$category <- "Cafe"

  cli::cli_alert_info("  parsed {nrow(out)} {section}")
  out
}


#' Build the CP-Pick filtered directory URL for a city + section
#' @noRd
cp_directory_url <- function(city, section = "restaurants") {
  if (!city %in% c("sydney", "melbourne")) {
    cli::cli_abort("Unknown city for Concrete Playground: {.val {city}}")
  }
  if (!section %in% c("restaurants", "bars", "cafes")) {
    cli::cli_abort("Unknown CP section: {.val {section}}")
  }
  paste0("https://concreteplayground.com/", city, "/", section,
         "?features%5B%5D=CP+Pick")
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
    # Concrete Playground's data-item JSON doesn't carry cuisine, but
    # the description prose almost always declares it ("Italian
    # trattoria", "Vietnamese pho shop"). Run the same prose_to_cuisine
    # rules used for Eater / CN Traveler.
    cuisine      = prose_to_cuisine(description),
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
