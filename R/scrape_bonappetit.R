#' Scrape Bon Appétit's annual "Best New Restaurants in America" list
#'
#' Bon Appétit publishes one annual article (~20 venues) spanning every
#' major US city. The 2025 article renders each venue as a pair of
#' `<strong>` blocks - the venue name followed by `<strong>CITY |</strong>`
#' (e.g. "NEW YORK CITY |", "WASHINGTON, DC |") - which is a stable
#' enough shape to parse without per-venue detail pages.
#'
#' Bon Appétit doesn't expose price, cuisine, address, or coordinates on
#' the list page (each venue links to a longer review article). We
#' pre-filter by city label so a Honolulu run doesn't burn API quota
#' geocoding 20 Pittsburgh / Atlanta venues; the geocoder then resolves
#' the survivors to coordinates.
#'
#' The 2024 article uses a different (Gatsby-rendered) layout - if you
#' want that year too, pass its URL via `extra_guides` and the parser
#' will fall through to the H2-only branch (city info won't be
#' available for those, so the geocoder gets a name-only query).
#'
#' @param city Character. Lowercase city slug (e.g. `"new-york"`,
#'   `"los-angeles"`, `"san-francisco"`, `"honolulu"`). Default
#'   `"new-york"`.
#' @param extra_guides Character vector. Extra BA article URLs to
#'   fetch alongside the default. Default `character()`.
#' @param use_cache Logical. Cache page responses for 24h. Default
#'   `FALSE`.
#'
#' @return A tibble with the standard scraper schema. Address and
#'   coordinates are `NA` - the geocoder fills them in.
#' @export
scrape_bonappetit <- function(city = "new-york",
                              extra_guides = character(),
                              use_cache = FALSE) {
  city <- validate_city_source(city, "bonappetit")
  cli::cli_h1("Scraping Bon App\u00E9tit: {city}")

  guides <- unique(c(bonappetit_default_guides(), extra_guides))
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
    bonappetit_parse_article(html_str, url)
  })
  results <- purrr::compact(results)
  if (length(results) == 0) {
    cli::cli_abort("No data scraped from any Bon App\u00E9tit guide.")
  }

  combined <- dplyr::bind_rows(results) |>
    dplyr::distinct(.data$name, .keep_all = TRUE)

  city_labels <- bonappetit_city_labels(city)
  filtered <- combined[
    is.na(combined$suburb) |
      vapply(combined$suburb, function(s) any(grepl(s, city_labels,
                                                     ignore.case = TRUE,
                                                     fixed = FALSE)),
             logical(1)),
    ,
    drop = FALSE
  ]

  cli::cli_alert_success(
    "Found {nrow(filtered)} venue{?s} in {city} ({nrow(combined)} total across the list)"
  )
  filtered
}


#' Default Bon Appétit article URL(s)
#'
#' Just the latest year. Older years used different layouts that
#' aren't worth maintaining a parser branch for; callers can pass
#' them as `extra_guides` if they want to opt in.
#' @noRd
bonappetit_default_guides <- function() {
  c("https://www.bonappetit.com/story/best-new-restaurants-2025")
}


#' Parse a Bon Appétit article into venue rows
#'
#' Looks for the 2025 strong-pair pattern first (venue name in a
#' `<strong>` block, followed within ~200 chars by a
#' `<strong>CITY |</strong>` label). Falls back to bare `<h2>NAME</h2>`
#' (the 2024 Gatsby layout) when the strong-pair pattern doesn't
#' fire - city info won't be available there.
#' @noRd
bonappetit_parse_article <- function(html_str, source_url) {
  rows <- bonappetit_parse_strong_pairs(html_str, source_url)
  if (length(rows) > 0) return(dplyr::bind_rows(rows))

  rows <- bonappetit_parse_bare_h2(html_str, source_url)
  if (length(rows) == 0) return(NULL)
  dplyr::bind_rows(rows)
}


#' 2025-style parser: strong VENUE -> strong CITY pairs
#' @noRd
bonappetit_parse_strong_pairs <- function(html_str, source_url) {
  # Two-stage regex: capture every <strong>...</strong> block, then
  # pair adjacent ones where the second matches the "CITY |" shape.
  strong_re <- "<strong>([^<]+)</strong>"
  m <- stringr::str_match_all(html_str, strong_re)[[1]]
  if (nrow(m) < 2) return(list())

  vals <- vapply(m[, 2], function(v) decode_html_entities(stringr::str_squish(v)),
                 character(1), USE.NAMES = FALSE)
  rows <- list()
  i <- 1L
  while (i < length(vals)) {
    name <- vals[i]
    next_val <- vals[i + 1L]
    # The location label always ends with " |" (pipe) on BA 2025.
    if (grepl("\\|\\s*$", next_val)) {
      # Strip the trailing " |" and any whitespace.
      city_label <- stringr::str_replace(next_val, "\\s*\\|\\s*$", "")
      if (nchar(name) >= 2 && nchar(name) <= 80 &&
          !grepl("^https?:|^www\\.|@", name)) {
        rows[[length(rows) + 1L]] <- bonappetit_row(
          name = bonappetit_title_case(name),
          suburb = city_label,
          url = source_url
        )
      }
      i <- i + 2L
    } else {
      i <- i + 1L
    }
  }
  rows
}


#' 2024-style fallback parser: bare H2 venue names, no city context
#' @noRd
bonappetit_parse_bare_h2 <- function(html_str, source_url) {
  h2 <- stringr::str_match_all(html_str, "<h2>([^<]+)</h2>")[[1]]
  if (nrow(h2) == 0) return(list())
  vals <- vapply(h2[, 2], function(v) decode_html_entities(stringr::str_squish(v)),
                 character(1), USE.NAMES = FALSE)
  # Drop duplicates (BA 2024 mirrors the venue list in the TOC) and
  # obviously-non-venue headings.
  vals <- vals[!duplicated(vals)]
  vals <- vals[nchar(vals) >= 2 & nchar(vals) <= 80]
  vals <- vals[!grepl("(?i)search by region|best new restaurants|table of contents",
                      vals, perl = TRUE)]
  if (length(vals) == 0) return(list())
  lapply(vals, function(name) {
    bonappetit_row(name = name, suburb = NA_character_, url = source_url)
  })
}


#' City labels that the BA list might use for the given city slug
#'
#' BA writes locations as "NEW YORK CITY", "WASHINGTON, DC",
#' "SAN FRANCISCO", etc. - sometimes neighborhood-anchored ("BROOKLYN")
#' for NYC. This mapping is permissive (any label match counts) so a
#' future year that uses e.g. "QUEENS" still lands in the NYC pipeline.
#' @noRd
bonappetit_city_labels <- function(city) {
  switch(city,
    `new-york`      = c("New York", "Brooklyn", "Queens", "Manhattan",
                        "Bronx", "Staten Island"),
    `los-angeles`   = c("Los Angeles", "LA", "Hollywood", "Santa Monica",
                        "Pasadena", "Culver City", "Venice"),
    `san-francisco` = c("San Francisco", "Oakland", "Berkeley"),
    honolulu        = c("Honolulu", "Oahu", "Waikiki"),
    london          = c("London"),
    cli::cli_abort("No Bon App\u00E9tit city labels configured for {.val {city}}")
  )
}


#' Build a tibble row in the package schema
#' @noRd
bonappetit_row <- function(name, suburb, url) {
  tibble::tibble(
    name         = name,
    suburb       = suburb,
    address      = NA_character_,
    cuisine      = NA_character_,
    category     = "Restaurant",
    description  = NA_character_,
    price_range  = NA_integer_,
    rating       = NA_real_,
    rating_scale = NA_character_,
    latitude     = NA_real_,
    longitude    = NA_real_,
    url          = url
  )
}


#' Title-case a venue name that BA emitted as ALL CAPS
#'
#' BA 2025 renders venue names in ALL CAPS for typographic effect
#' ("BAAN MAE", "HA'S SNACK BAR"). Normalise to standard title case
#' so the same venue from BA matches the same venue from other sources
#' during dedup (which compares lowercased + ASCII-folded forms, so
#' the exact case doesn't change dedup behavior - it's just what we
#' display in popups).
#' @noRd
bonappetit_title_case <- function(s) {
  if (is.na(s) || !nzchar(s)) return(s)
  if (s == tolower(s) || s != toupper(s)) return(s)  # already mixed case
  # Title-case each word; keep small connector words lowercase
  tools::toTitleCase(tolower(s))
}
