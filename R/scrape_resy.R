#' Scrape Resy's monthly Hit List per city
#'
#' Resy maintains a rolling, monthly-updated "Hit List" of the top
#' restaurants in each city it covers (NYC, LA, plus historical lists
#' for other cities). The article HTML is dense with structured data:
#' every venue is rendered as an `<article class="teaser2">` block
#' with `data-lat` / `data-lng` attributes on the article tag, the
#' venue name + rank inside an `<h3 class="teaser2-title">`, and a
#' `<ul class="teaser2-meta">` carrying the neighborhood, cuisine and
#' price tier as three `<li>` items.
#'
#' That means a single fetch yields name, neighborhood, cuisine,
#' price, rank, coordinates and the Resy booking URL - no per-venue
#' detail fetch needed (the per-venue pages are Angular-rendered
#' anyway, so they'd require a headless browser).
#'
#' @param city Character. Lowercase city slug. Currently supported:
#'   `"new-york"`, `"los-angeles"`. Default `"new-york"`.
#' @param use_cache Logical. Cache the Hit List page for 24h via the
#'   package's internal `cached_fetch()` helper. Default `FALSE`.
#'
#' @return A tibble with the standard scraper schema. Latitude /
#'   longitude come straight from the page DOM so no geocoder call is
#'   needed for these rows.
#' @export
scrape_resy <- function(city = "new-york", use_cache = FALSE) {
  city <- validate_city_source(city, "resy")
  cli::cli_h1("Scraping Resy Hit List: {city}")

  url <- resy_url(city)
  cli::cli_alert_info("Fetching {.url {url}}")
  Sys.sleep(RATE_LIMIT_SECS)
  html_str <- cached_fetch(url, use_cache = use_cache)

  result <- resy_parse(html_str, url)
  if (is.null(result) || nrow(result) == 0) {
    cli::cli_abort(
      "No Resy venues parsed - has the Hit List article shape changed?"
    )
  }

  cli::cli_alert_success("Found {nrow(result)} venue{?s}")
  result
}


#' Per-city Hit List URL
#'
#' Resy uses different blog paths per city, all on `blog.resy.com`.
#' Each path is a rolling article (no year/month in the slug), so
#' these stay stable across monthly refreshes.
#' @noRd
resy_url <- function(city) {
  switch(city,
    `new-york`    = "https://blog.resy.com/the-hit-list/nyc-restaurants/",
    `los-angeles` = "https://blog.resy.com/the-hit-list/la-restaurants/",
    cli::cli_abort("No Resy Hit List URL configured for {.val {city}}")
  )
}


#' Parse the Hit List article HTML into a tibble of venues
#'
#' Walks every `<article class="teaser2" data-lat="..." data-lng="...">`
#' block, pulling: name + rank from `<h3 class="teaser2-title">`,
#' neighborhood / cuisine / price from the three `<li>` items in
#' `<ul class="teaser2-meta">`. The rank is dropped from the name (Resy
#' renders venues as "1. Kidilum" but the rank isn't useful for our
#' downstream tooling).
#' @noRd
resy_parse <- function(html_str, source_url) {
  # Each venue block is delimited by the teaser2 article opening tag
  # carrying its lat/lng. Capture the article opener + a generous chunk
  # of following HTML so we can pull the name and meta list out.
  article_re <- '<article class="teaser2[^"]*"\\s+data-lat="(-?[0-9.]+)"\\s+data-lng="(-?[0-9.]+)">([\\s\\S]*?)</article>'
  m <- stringr::str_match_all(html_str, article_re)[[1]]
  if (nrow(m) == 0) return(NULL)

  rows <- lapply(seq_len(nrow(m)), function(i) {
    lat   <- suppressWarnings(as.numeric(m[i, 2]))
    lng   <- suppressWarnings(as.numeric(m[i, 3]))
    block <- m[i, 4]

    name_match <- stringr::str_match(
      block,
      '<h3 class="teaser2-title[^"]*">\\s*<span>\\s*<a[^>]*>([^<]+)</a>'
    )
    raw_name <- name_match[1, 2]
    if (is.na(raw_name)) return(NULL)
    name <- resy_strip_rank(decode_html_entities(stringr::str_squish(raw_name)))
    if (!nzchar(name)) return(NULL)

    href_match <- stringr::str_match(
      block,
      '<a href="(https://resy\\.com/cities/[^"]+)"'
    )
    venue_url <- href_match[1, 2]

    meta_li <- stringr::str_match_all(
      block, "<li>([^<]+)</li>"
    )[[1]]
    meta_vals <- if (nrow(meta_li) > 0) {
      # decode_html_entities is scalar-only - vapply over the column
      vapply(meta_li[, 2], function(v) {
        stringr::str_squish(decode_html_entities(v))
      }, character(1), USE.NAMES = FALSE)
    } else {
      character()
    }

    # Resy renders the meta list as [neighborhood, cuisine, price].
    # Order is stable in the markup; positional extraction is safer
    # than guessing by content (cuisine "Bar" could otherwise be
    # mistaken for a neighborhood).
    suburb  <- if (length(meta_vals) >= 1) meta_vals[1] else NA_character_
    cuisine <- if (length(meta_vals) >= 2) meta_vals[2] else NA_character_
    price_s <- if (length(meta_vals) >= 3) meta_vals[3] else NA_character_

    tibble::tibble(
      name         = name,
      suburb       = suburb,
      address      = NA_character_,
      cuisine      = cuisine,
      category     = "Restaurant",
      description  = NA_character_,
      price_range  = resy_price_to_int(price_s),
      rating       = NA_real_,
      rating_scale = NA_character_,
      latitude     = lat,
      longitude    = lng,
      url          = venue_url %||% source_url
    )
  })
  rows <- purrr::compact(rows)
  if (length(rows) == 0) return(NULL)
  dplyr::bind_rows(rows)
}


#' Drop the leading rank prefix Resy puts on each venue title
#'
#' Resy titles read "1. Kidilum", "2. Odo East Village", etc. The rank
#' tracks article position - useful editorially but not downstream -
#' so strip it. Defensive against `NA` and non-rank-prefixed names
#' (Resy occasionally publishes side categories without ranks).
#' @noRd
resy_strip_rank <- function(name) {
  if (is.na(name)) return(name)
  stringr::str_replace(name, "^\\s*\\d+\\.\\s*", "")
}


#' Convert Resy's "$" / "$$" / "$$$" / "$$$$" string to a 1-4 integer
#'
#' Returns `NA_integer_` for missing / non-dollar inputs. Same shape
#' as the converters in [scrape_michelin()] etc. so the merged price
#' field stays consistent across sources.
#' @noRd
resy_price_to_int <- function(s) {
  if (is.na(s) || !nzchar(s)) return(NA_integer_)
  n <- nchar(gsub("[^$]", "", s))
  if (n < 1L) return(NA_integer_)
  as.integer(min(n, 4L))
}
