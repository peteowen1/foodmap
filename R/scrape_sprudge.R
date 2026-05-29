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
#' @param city Character. Supported cities are `"san-francisco"`,
#'   `"new-york"`, `"los-angeles"`, `"london"`, `"sydney"`, and
#'   `"melbourne"`. Default `"san-francisco"`.
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
    sprudge_parse_article(html_str, u, city = city)
  }) |> purrr::compact()

  if (length(rows) == 0) {
    cli::cli_abort("No Sprudge articles parsed successfully.")
  }

  combined <- dplyr::bind_rows(rows) |>
    dplyr::distinct(.data$name, .keep_all = TRUE)
  cli::cli_alert_success("Found {nrow(combined)} cafe{?s}")
  combined
}


#' Default search queries used to surface city cafe articles
#'
#' Each city's queries follow the same three-template pattern that
#' surfaces the most cafe-spotlight coverage:
#'   coffee design <city>
#'   sprudge maps <city>
#'   build-outs <city>
#'
#' Plus an open <city> cafe query to catch one-off founder profiles.
#' @noRd
sprudge_default_queries <- function(city) {
  # Sprudge's URL slug for each city - matches the substring we look
  # for in article URLs via sprudge_is_spotlight().
  city_phrase <- switch(city,
    `san-francisco` = "san francisco",
    `new-york`      = "new york",
    `los-angeles`   = "los angeles",
    london          = "london",
    sydney          = "sydney",
    melbourne       = "melbourne",
    cli::cli_abort("No default Sprudge queries for {.val {city}}")
  )
  c(
    paste("coffee design", city_phrase),
    paste("sprudge maps", city_phrase),
    paste("build-outs coffee", city_phrase),
    paste("build-outs", city_phrase),
    paste(city_phrase, "cafe")
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


#' Decide whether a Sprudge article URL looks like a cafe spotlight
#'
#' Sprudge's editorial structure varies by region:
#'
#'   * US cities (SF/NY/LA) follow a tidy three-series pattern:
#'     "Coffee Design", "Sprudge Maps", "Build-Outs" - each URL slug
#'     starts with one of those series tokens. Positive-filter on the
#'     series tokens AND the city slug.
#'
#'   * AU/UK cities don't use the series prefixes - cafe spotlights
#'     use venue-name slugs ("rumble-coffee-roasters-melbourne-X",
#'     "allpress-melbourne-X", "filter-coffee-melbourne-X"). Positive
#'     filtering would miss most of these. Instead, take any URL
#'     containing the city slug, then negative-filter the known
#'     non-spotlight categories: multi-venue guides ("where-to-",
#'     "*-guide", "guide-to-"), news ("mainstream-media", "tragedy",
#'     "battle"), events ("expo", "sca-"), celebrity gossip
#'     ("sweeney"), opinion pieces ("running-out", "reflection"),
#'     and wholesale-industry stories.
#' @noRd
sprudge_is_spotlight <- function(urls, city) {
  if (length(urls) == 0) return(logical())
  city_slug <- sprudge_city_slug(city)
  if (is.null(city_slug)) return(rep(FALSE, length(urls)))

  city_in_url <- grepl(city_slug, urls, fixed = TRUE)

  if (city %in% c("san-francisco", "new-york", "los-angeles")) {
    series_re <- "/(coffee-design|sprudge-maps|build-outs)[^/]*"
    return(grepl(series_re, urls) & city_in_url)
  }

  # AU / UK path: take everything in-city, exclude obvious non-venue articles.
  not_spotlight_re <- paste0(
    "where-to-(drink|go|find)|",     # multi-venue travel articles
    "-coffee-guide|coffee-guide-|",  # city-wide coffee guides
    "guide-to-|",                    # multi-venue guides
    "mainstream-media|",             # industry/press meta
    "reflection|tragedy|",           # opinion / news
    "battle|expo|sca-|",             # events / competitions
    "running-out|fantasies|",        # opinion pieces / clickbait
    "wholesale|",                    # industry trade stories
    "sweeney|",                      # celebrity gossip
    "-hours-|caffeinated-hours|",    # "24 hours in" travelogues
    "best-(cafes|coffees|coffee-shops)" # ranked listicles - we want individual spotlights
  )
  city_in_url & !grepl(not_spotlight_re, urls)
}


#' URL-slug form of each city used in Sprudge article paths
#' @noRd
sprudge_city_slug <- function(city) {
  switch(city,
    `san-francisco` = "san-francisco",
    `new-york`      = "new-york",
    `los-angeles`   = "los-angeles",
    london          = "london",
    sydney          = "sydney",
    melbourne       = "melbourne",
    NULL
  )
}


#' Human-readable city name used in Sprudge og:title strings
#'
#' Used by `sprudge_clean_name()` to strip the trailing location clause
#' off article titles for cities other than SF.
#' @noRd
sprudge_city_display <- function(city) {
  switch(city,
    `san-francisco` = "San Francisco",
    `new-york`      = "New York",
    `los-angeles`   = "Los Angeles",
    london          = "London",
    sydney          = "Sydney",
    melbourne       = "Melbourne",
    NULL
  )
}


#' Parse a Sprudge article into a single venue row
#' @noRd
sprudge_parse_article <- function(html_str, url, city = "san-francisco") {
  og_title <- stringr::str_match(
    html_str, 'property="og:title"\\s+content="([^"]+)"'
  )[1, 2]
  if (is.na(og_title)) return(NULL)

  name <- sprudge_clean_name(og_title, city = city)
  if (is.na(name) || !nzchar(name)) return(NULL)

  og_desc <- stringr::str_match(
    html_str, 'property="og:description"\\s+content="([^"]+)"'
  )[1, 2]
  if (!is.na(og_desc)) og_desc <- decode_html_entities(og_desc)

  # The suburb column is the city display name - "San Francisco",
  # "New York", etc. - so downstream consumers can group by city
  # without needing to look at the URL. Real neighbourhoods come
  # from the geocoder when it resolves the address.
  tibble::tibble(
    name         = name,
    suburb       = sprudge_city_display(city) %||% NA_character_,
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
#' lop off the trailing location clause (" In <city>..."). The location
#' clause varies by city so the city name pattern is built dynamically.
#' @noRd
sprudge_clean_name <- function(title, city = "san-francisco") {
  if (is.na(title)) return(NA_character_)
  title <- decode_html_entities(title)
  title <- stringr::str_trim(title)

  # Drop trailing " | Sprudge Coffee" suffix
  title <- stringr::str_replace(title, "\\s*\\|\\s*Sprudge\\s+Coffee\\s*$", "")

  # Drop leading series prefix. Sprudge uses several series for
  # spotlights: "Coffee Design" + "Sprudge Maps [Spotlight]" +
  # "Build-Outs [Of X]" are the long-running US patterns; "Nice Package"
  # is the coffee-packaging-focused series that turns up across regions
  # including AU. Build-Outs variants include "Of Summer", "Of Fall",
  # "Of Coffee", etc.
  title <- stringr::str_replace(
    title,
    paste0(
      "^(Coffee Design|Sprudge Maps Spotlight|Sprudge Maps|",
      "Build-Outs(?:\\s+Of\\s+\\w+)?|Build-Outs|",
      "Nice Package)[:\\s-]+"
    ),
    ""
  )

  # Build a case-insensitive name pattern for the city. The display
  # form ("San Francisco", "New York") matches what appears in titles.
  city_display <- sprudge_city_display(city) %||% "San Francisco"
  city_re <- gsub("\\s+", "\\\\s+", city_display)

  # Drop trailing location clause: " In <city>..." / " of <city>..." /
  # " at <city>..."; also "(<city>)" and ", <city>, ..." variants.
  title <- stringr::str_replace(
    title,
    paste0("(?i)\\s+(?:in|of|at)\\s+", city_re, ".*$"),
    ""
  )
  title <- stringr::str_replace(title,
                                paste0(",\\s*", city_re, ".*$"), "")
  title <- stringr::str_replace(title,
                                paste0("\\s*\\(", city_re, "\\).*$"), "")
  # Also strip a bare trailing city name (no preposition). AU articles
  # often title themselves "Rumble Coffee Roasters Melbourne" with no
  # "in"/"of"/"at" before the city.
  title <- stringr::str_replace(title,
                                paste0("(?i)\\s+", city_re, "\\s*$"), "")

  # Build-Outs articles often title themselves "Cafe's Third Location"
  # or "Cafe's New Location" -- the geocoder needs the cafe name only.
  title <- stringr::str_replace(
    title,
    "(?i)['\u2019]s\\s+(?:new|first|second|third|fourth|fifth|nth|next)\\s+location.*$",
    ""
  )

  title <- stringr::str_squish(title)
  if (!nzchar(title)) return(NA_character_)
  title
}
