#' Scrape Sprudge SF coffee shop spotlights
#'
#' Sprudge is a specialty-coffee publication that covers individual
#' cafes through a few recurring article series ("Coffee Design",
#' "Sprudge Maps Spotlight"). Unlike Eater/Infatuation, there is no
#' single roundup page with an embedded venue list -- each cafe gets
#' its own article. This scraper harvests cafe names by querying
#' Sprudge's WordPress search for SF terms, filtering returned URLs to
#' the venue-spotlight patterns, and pulling the cafe name from each
#' article's `og:title` meta tag.
#'
#' Addresses and coordinates are intentionally left blank -- Sprudge
#' rarely publishes structured location data and the geocoding pipeline
#' resolves "<name> San Francisco" to coords via Google Places.
#' Cuisine is set to `"Coffee"` for every venue so the cafe filter on
#' the rendered map picks them up without further tagging.
#'
#' @param city Character. Currently only `"san-francisco"`. Default
#'   `"san-francisco"`.
#' @param extra_guides Character vector of extra Sprudge article URLs
#'   to harvest on top of the search-driven defaults. Useful for
#'   one-off spotlight articles that don't match the default search
#'   queries (e.g. founder profiles, expansion stories).
#' @param use_cache Logical. Cache HTTP responses for 24h via
#'   `cached_fetch()`. Default `FALSE`.
#'
#' @return A tibble with the standard scraper schema. Address /
#'   latitude / longitude are `NA` -- they get filled in downstream by
#'   `geocode_restaurants()`.
#' @export
scrape_sprudge <- function(city = "san-francisco",
                           extra_guides = character(),
                           use_cache = FALSE) {
  city <- validate_city_source(city, "sprudge")
  cli::cli_h1("Scraping Sprudge: {city}")

  # Sprudge's WordPress search returns ~10-15 results per query and
  # truncates beyond that, so issue a few different queries to surface
  # both Coffee Design and Sprudge Maps articles. The intersection of
  # results is the SF cafe set.
  queries <- sprudge_default_queries(city)
  cli::cli_alert_info("Running {length(queries)} search quer{?y/ies}")

  search_urls <- purrr::map(queries, function(q) {
    sprudge_search_urls(q, use_cache = use_cache)
  }) |> unlist() |> unique()

  cli::cli_alert_info("Found {length(search_urls)} candidate article URL{?s}")

  # Filter to known cafe-spotlight URL patterns.
  spotlight_urls <- search_urls[sprudge_is_spotlight(search_urls, city)]
  spotlight_urls <- unique(c(spotlight_urls, extra_guides))

  if (length(spotlight_urls) == 0) {
    cli::cli_warn("No Sprudge cafe spotlight articles matched.")
    return(NULL)
  }
  cli::cli_alert_info("Fetching {length(spotlight_urls)} cafe spotlight{?s}")

  rows <- purrr::map(spotlight_urls, function(u) {
    Sys.sleep(RATE_LIMIT_SECS)
    cli::cli_alert_info("  {.url {u}}")
    html_str <- tryCatch(
      cached_fetch(u, use_cache = use_cache),
      error = function(e) {
        cli::cli_warn("  failed: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(html_str)) return(NULL)
    sprudge_parse_article(html_str, u)
  }) |> purrr::compact()

  if (length(rows) == 0) {
    cli::cli_abort("No Sprudge articles parsed successfully.")
  }

  combined <- dplyr::bind_rows(rows) |>
    dplyr::distinct(.data$name, .keep_all = TRUE)
  cli::cli_alert_success("Found {nrow(combined)} cafe{?s}")
  combined
}


#' Default search queries used to surface SF cafe articles
#' @noRd
sprudge_default_queries <- function(city) {
  switch(city,
    `san-francisco` = c(
      "coffee design san francisco",
      "sprudge maps san francisco",
      "san francisco cafe",
      "build-outs coffee san francisco",
      "build-outs san francisco"
    ),
    cli::cli_abort("No default Sprudge queries for {.val {city}}")
  )
}


#' Run a single Sprudge WordPress search and return article URLs found
#' on the first results page. Sprudge does not paginate cleanly via
#' `?paged=N` so we accept the first page only and rely on multiple
#' queries for breadth.
#' @noRd
sprudge_search_urls <- function(query, use_cache = FALSE) {
  url <- paste0("https://sprudge.com/?s=", utils::URLencode(query))
  html_str <- tryCatch(
    cached_fetch(url, use_cache = use_cache),
    error = function(e) {
      cli::cli_warn("Sprudge search {.val {query}} failed: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(html_str)) return(character())

  hrefs <- stringr::str_match_all(
    html_str,
    'href="(https://sprudge\\.com/[^"]+\\.html)"'
  )[[1]]
  if (nrow(hrefs) == 0) return(character())
  unique(hrefs[, 2])
}


#' Decide whether a Sprudge article URL looks like an SF cafe spotlight
#'
#' Restricts to three reliable cafe-spotlight series:
#' "Coffee Design", "Sprudge Maps", and "Build-Outs" (new-cafe
#' openings). All three series spotlight a single venue per article.
#' Combined with the san-francisco URL token, this filters out
#' festival, conference, lawsuit, and other unrelated SF coverage.
#' @noRd
sprudge_is_spotlight <- function(urls, city) {
  if (length(urls) == 0) return(logical())
  if (!identical(city, "san-francisco")) return(rep(FALSE, length(urls)))
  series_re <- "/(coffee-design|sprudge-maps|build-outs)[^/]*"
  city_re   <- "san-francisco"
  grepl(series_re, urls) & grepl(city_re, urls)
}


#' Parse a Sprudge article into a single venue row
#' @noRd
sprudge_parse_article <- function(html_str, url) {
  og_title <- stringr::str_match(
    html_str, 'property="og:title"\\s+content="([^"]+)"'
  )[1, 2]
  if (is.na(og_title)) return(NULL)

  name <- sprudge_clean_name(og_title)
  if (is.na(name) || !nzchar(name)) return(NULL)

  og_desc <- stringr::str_match(
    html_str, 'property="og:description"\\s+content="([^"]+)"'
  )[1, 2]
  if (!is.na(og_desc)) og_desc <- decode_html_entities(og_desc)

  tibble::tibble(
    name         = name,
    suburb       = "San Francisco",
    address      = NA_character_,
    cuisine      = "Coffee",
    category     = "Cafe",
    description  = og_desc,
    price_range  = NA_integer_,
    rating       = NA_real_,
    rating_scale = NA_character_,
    latitude     = NA_real_,
    longitude    = NA_real_,
    url          = url
  )
}


#' Clean a cafe name out of a Sprudge article og:title
#'
#' Sprudge titles follow predictable forms:
#'   "Coffee Design: Saint Frank Coffee In San Francisco, CA | Sprudge Coffee"
#'   "Sprudge Maps Spotlight: Round House Cafe by Equator Coffees In San Francisco, CA"
#'
#' Strategy: strip the trailing site brand, strip the leading series
#' prefix ("Coffee Design:" / "Sprudge Maps Spotlight:" / etc.), then
#' lop off the trailing location clause (" In San Francisco...").
#' @noRd
sprudge_clean_name <- function(title) {
  if (is.na(title)) return(NA_character_)
  title <- decode_html_entities(title)
  title <- stringr::str_trim(title)

  # Drop trailing " | Sprudge Coffee" suffix
  title <- stringr::str_replace(title, "\\s*\\|\\s*Sprudge\\s+Coffee\\s*$", "")

  # Drop leading series prefix. The Build-Outs series uses several
  # variants ("Of Summer", "Of Fall", "Of Coffee", just "Build-Outs").
  title <- stringr::str_replace(
    title,
    paste0(
      "^(Coffee Design|Sprudge Maps Spotlight|Sprudge Maps|",
      "Build-Outs(?:\\s+Of\\s+\\w+)?|Build-Outs)[:\\s-]+"
    ),
    ""
  )

  # Drop trailing location clause: " In San Francisco, CA" / " In SF" /
  # " (San Francisco)" / ", San Francisco, CA". Matches both " In " and
  # " in " forms.
  title <- stringr::str_replace(
    title,
    "(?i)\\s+(?:in|of|at)\\s+san\\s+francisco.*$",
    ""
  )
  title <- stringr::str_replace(title, ",\\s*San\\s+Francisco.*$", "")
  title <- stringr::str_replace(title, "\\s*\\(San\\s+Francisco\\).*$", "")

  # Build-Outs articles often title themselves "Cafe's Third Location"
  # or "Cafe's New Location" -- the geocoder needs the cafe name only.
  title <- stringr::str_replace(
    title,
    "(?i)['’]s\\s+(?:new|first|second|third|fourth|fifth|nth|next)\\s+location.*$",
    ""
  )

  title <- stringr::str_squish(title)
  if (!nzchar(title)) return(NA_character_)
  title
}
