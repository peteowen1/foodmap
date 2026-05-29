#' Scrape Broadsheet's editorial cafe / bar / pub guides
#'
#' Complements `scrape_broadsheet()` (which only hits the restaurant
#' hotlist API). Broadsheet's `/{city}/guides/{slug}` pages are curated
#' editorial best-of lists for cafes, bars, cocktail spots, pubs, etc.
#' Each guide page embeds a `schema.org` `ItemList` JSON-LD block whose
#' `itemListElement` array carries clean venue records:
#'
#'   * `@type`      - `CafeOrCoffeeShop` / `BarOrPub` / `Restaurant`
#'   * `name`, `url`, `description`, `image`, `telephone`
#'   * `priceRange` (integer 1-4)
#'   * `address`    - `streetAddress`, `addressLocality` (suburb),
#'                    `postalCode`, `addressRegion`
#'
#' That's everything the standard pipeline schema needs; no per-venue
#' detail fetch is required.
#'
#' Coverage is curated, not enumerated - each city has a hand-picked
#' list of cafe + bar guides. Discoverable via
#' `https://www.broadsheet.com.au/sitemap/{city}/guides` if a new
#' guide is published and you want to fold it in.
#'
#' @param city Character. `"sydney"` or `"melbourne"`. Default
#'   `"sydney"`.
#' @param use_cache Logical. Cache HTTP responses for 24h via
#'   `cached_fetch()`. Default `FALSE`.
#'
#' @return A tibble with the standard scraper schema. `category` is
#'   set from the JSON-LD `@type` (`"Cafe"`, `"Bar"`, or `"Restaurant"`).
#' @export
scrape_broadsheet_guides <- function(city = "sydney", use_cache = FALSE) {
  city <- validate_city_source(city, "broadsheet_guides")
  cli::cli_h1("Scraping Broadsheet Guides: {city}")

  guides <- broadsheet_guides_for_city(city)
  cli::cli_alert_info("Fetching {length(guides)} guide{?s}")

  rows <- purrr::map(guides, function(g) {
    Sys.sleep(RATE_LIMIT_SECS)
    url <- broadsheet_guide_url(city, g$slug)
    cli::cli_alert_info("  {.url {url}}")
    html_str <- tryCatch(
      cached_fetch(url, use_cache = use_cache),
      error = function(e) {
        cli::cli_warn("  failed: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(html_str)) return(NULL)
    bg_parse_guide(html_str, guide = g, city = city)
  }) |> purrr::compact()

  if (length(rows) == 0) {
    cli::cli_abort("No Broadsheet guides parsed successfully.")
  }

  combined <- dplyr::bind_rows(rows) |>
    # Same venue can appear across multiple guides (e.g. a cocktail
    # bar in both "best-cocktails" and "best-rooftop-bars"). Keep
    # the first occurrence; guides are ordered cafe-first then bar-
    # first so cafes win when a venue legitimately serves both.
    dplyr::distinct(.data$name, .data$suburb, .keep_all = TRUE)
  cli::cli_alert_success("Found {nrow(combined)} venue{?s}")
  combined
}


#' Per-city curated list of guide slugs + their implied category/cuisine
#'
#' Each entry is a list of (slug, category, cuisine). The category sets
#' the row's `category` column directly; cuisine is a default that the
#' JSON-LD-provided value (rarely present) overrides if available.
#'
#' Refresh via the sitemap when Broadsheet adds new guides:
#'   curl https://www.broadsheet.com.au/sitemap/sydney/guides
#'
#' Skipped deliberately: `cafesmart` (charity event), `whats-open-*`
#' (holiday-specific listicles that rotate annually).
#' @noRd
broadsheet_guides_for_city <- function(city) {
  switch(city,
    sydney = list(
      list(slug = "best-cafes",         category = "Cafe", cuisine = "Cafe"),
      list(slug = "best-coffee",        category = "Cafe", cuisine = "Coffee"),
      list(slug = "new-cafes",          category = "Cafe", cuisine = "Cafe"),
      list(slug = "best-breakfast",     category = "Cafe", cuisine = "Breakfast"),
      list(slug = "best-boozy-brunches", category = "Cafe", cuisine = "Brunch"),
      list(slug = "bars-cbd",           category = "Bar",  cuisine = "Bar"),
      list(slug = "best-cocktails",     category = "Bar",  cuisine = "Cocktail Bar"),
      list(slug = "best-rooftop-bars",  category = "Bar",  cuisine = "Rooftop Bar"),
      list(slug = "new-bars",           category = "Bar",  cuisine = "Bar"),
      list(slug = "pubs-sydney",        category = "Bar",  cuisine = "Pub")
    ),
    melbourne = list(
      list(slug = "best-cafes-brunswick",       category = "Cafe", cuisine = "Cafe"),
      list(slug = "best-cafes-melbournes-cbd",  category = "Cafe", cuisine = "Cafe"),
      list(slug = "best-coffee",                category = "Cafe", cuisine = "Coffee"),
      list(slug = "new-cafes",                  category = "Cafe", cuisine = "Cafe"),
      list(slug = "bars-melbourne",             category = "Bar",  cuisine = "Bar"),
      list(slug = "best-cocktail-bars-melbourne", category = "Bar", cuisine = "Cocktail Bar"),
      list(slug = "beer-gardens",               category = "Bar",  cuisine = "Beer Garden"),
      list(slug = "best-bars-brunswick",        category = "Bar",  cuisine = "Bar"),
      list(slug = "best-hidden-bars",           category = "Bar",  cuisine = "Hidden Bar"),
      list(slug = "listening-bars",             category = "Bar",  cuisine = "Listening Bar"),
      list(slug = "new-bars",                   category = "Bar",  cuisine = "Bar"),
      list(slug = "pubs",                       category = "Bar",  cuisine = "Pub"),
      list(slug = "rooftop-bars-melbourne",     category = "Bar",  cuisine = "Rooftop Bar")
    ),
    cli::cli_abort("No Broadsheet guides configured for {.val {city}}")
  )
}


#' Build a Broadsheet guide URL from city + slug
#' @noRd
broadsheet_guide_url <- function(city, slug) {
  paste0("https://www.broadsheet.com.au/", city, "/guides/", slug)
}


#' Parse one Broadsheet guide article's JSON-LD ItemList into rows
#'
#' Broadsheet pages embed three JSON-LD blocks: an Article record, a
#' BreadcrumbList, and the venue ItemList. We want the ItemList - it's
#' the only one with @type == "ItemList" that has items, so picking it
#' by @type is unambiguous.
#'
#' Each item is a schema.org Place subtype (CafeOrCoffeeShop, BarOrPub,
#' Restaurant) with the venue fields we need. Missing values stay NA;
#' the downstream geocoder fills coords from address.
#' @noRd
bg_parse_guide <- function(html_str, guide, city) {
  page <- rvest::read_html(html_str)
  scripts <- rvest::html_elements(
    page, "script[type='application/ld+json']"
  ) |> rvest::html_text()

  item_list <- NULL
  for (txt in scripts) {
    parsed <- tryCatch(
      jsonlite::fromJSON(txt, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(parsed)) next
    if (identical(parsed[["@type"]], "ItemList") &&
        length(parsed$itemListElement) > 0) {
      item_list <- parsed$itemListElement
      break
    }
  }
  if (is.null(item_list)) return(NULL)

  rows <- lapply(item_list, function(entry) {
    bg_item_to_row(entry$item, guide = guide, city = city)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)
  dplyr::bind_rows(rows)
}


#' Turn a single ItemList `item` into a tibble row
#'
#' `priceRange` arrives as either an integer (1-4) or a `"$$"`-style
#' string. We coerce numerics directly and count `$` for strings.
#' Anything else stays NA.
#' @noRd
bg_item_to_row <- function(item, guide, city) {
  if (!is.list(item)) return(NULL)
  name <- item$name %||% NA_character_
  if (is.na(name) || !nzchar(name)) return(NULL)

  addr <- item$address %||% list()
  street   <- addr$streetAddress %||% NA_character_
  suburb   <- addr$addressLocality %||% NA_character_
  postcode <- addr$postalCode %||% NA_character_
  full_address <- paste(
    Filter(function(x) !is.na(x) && nzchar(x),
           c(street, suburb, postcode)),
    collapse = ", "
  )
  if (!nzchar(full_address)) full_address <- NA_character_

  price_int <- bg_coerce_price(item$priceRange)

  category <- guide$category %||% "Restaurant"
  cuisine  <- guide$cuisine  %||% NA_character_

  tibble::tibble(
    name         = name,
    suburb       = suburb,
    address      = full_address,
    cuisine      = cuisine,
    category     = category,
    description  = item$description %||% NA_character_,
    price_range  = price_int,
    rating       = NA_real_,
    rating_scale = NA_character_,
    latitude     = NA_real_,
    longitude    = NA_real_,
    url          = item$url %||% NA_character_
  )
}


#' Coerce Broadsheet's priceRange field to a 1-4 integer
#' @noRd
bg_coerce_price <- function(x) {
  if (is.null(x)) return(NA_integer_)
  if (is.numeric(x)) {
    v <- as.integer(x)
    if (is.na(v) || v < 1L || v > 4L) return(NA_integer_)
    return(v)
  }
  if (is.character(x) && grepl("^\\$+$", x)) {
    return(as.integer(min(nchar(x), 4L)))
  }
  NA_integer_
}
