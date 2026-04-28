#' Scrape SMH/Age Good Food Guide reviews
#'
#' Fetches restaurant reviews from the Good Food reviews section at
#' smh.com.au/goodfood/reviews. Uses chromote to click "Show more" and
#' load all review links, then fetches each detail page for venue name,
#' score (/20 with .5 precision), cuisine, price, address, and suburb.
#'
#' @param city Character. One of "sydney", "melbourne". Default `"sydney"`.
#' @param use_cache Logical. If `TRUE`, cache the per-review detail pages
#'   locally to avoid re-fetching on every run. The chromote-driven listing
#'   page is not cached (it dynamically expands all reviews). Default `FALSE`.
#'
#' @return A tibble with columns: name, suburb, address, cuisine, category,
#'   description, price_range, cost_range, rating, rating_scale, hats,
#'   review_date, phone, website, latitude, longitude, url.
#' @export
scrape_good_food_guide <- function(city = "sydney", use_cache = FALSE) {
  city <- validate_city_source(city, "good_food_guide")
  rlang::check_installed("chromote", reason = "for Good Food Guide scraping")
  cli::cli_h1("Scraping Good Food Guide: {city}")

  # Phase 1: Get all review URLs from listing page via chromote
  links <- gfg_fetch_review_links(city)

  if (length(links) == 0) {
    cli::cli_abort("No review links found on Good Food Guide page.")
  }

  # Filter to city-specific articles
  city_slug <- switch(city,
    sydney    = "sydney-eating-out",
    melbourne = "melbourne-eating-out"
  )
  links <- links[grepl(city_slug, links)]
  cli::cli_alert_info("Found {length(links)} {city} review link{?s}")

  if (length(links) == 0) {
    cli::cli_abort("No {city} reviews found.")
  }

  # Phase 2: Fetch each detail page for structured data
  cli::cli_alert_info("Fetching detail pages...")
  n_links <- length(links)
  raw_results <- purrr::imap(links, function(url, idx) {
    if (idx %% 20 == 0) cli::cli_alert_info("  ...{idx}/{n_links}")
    Sys.sleep(RATE_LIMIT_SECS)
    tryCatch(gfg_fetch_detail(url, use_cache = use_cache), error = function(e) NULL)
  })
  n_failed <- sum(purrr::map_lgl(raw_results, is.null))
  if (n_failed > 0) {
    cli::cli_warn("{n_failed}/{n_links} detail page{?s} failed to fetch or parse")
  }
  results <- purrr::compact(raw_results)

  if (length(results) == 0) {
    cli::cli_abort("Could not extract any venue data from Good Food Guide.")
  }

  result <- dplyr::bind_rows(results)
  cli::cli_alert_success(
    "Found {nrow(result)} venue{?s} ({sum(!is.na(result$rating))} with scores)"
  )
  result
}

#' Fetch all review URLs from the Good Food reviews listing page
#' @noRd
gfg_fetch_review_links <- function(city) {
  cli::cli_alert_info("Loading review listing via chromote...")

  b <- chromote::ChromoteSession$new()
  on.exit(b$close(), add = TRUE)

  b$Page$navigate(gfg_reviews_url(city))
  # Wait for page load event, falling back to fixed sleep
  tryCatch(
    b$Page$loadEventFired(timeout = 10),
    error = function(e) Sys.sleep(5)
  )

  # Click "Show more" repeatedly to load all reviews
  cli::cli_alert_info("Expanding all reviews...")
  for (i in 1:20) {
    result <- b$Runtime$evaluate("
      (function() {
        var btn = Array.from(document.querySelectorAll('button'))
          .find(function(b) { return b.textContent.includes('Show more'); });
        if (btn) { btn.click(); return true; }
        return false;
      })()
    ")
    if (!isTRUE(result$result$value)) break
    Sys.sleep(1.5)
  }

  # Extract all article links
  links_json <- b$Runtime$evaluate("
    (function() {
      var links = document.querySelectorAll('a[href$=\".html\"]');
      var urls = [];
      for (var i = 0; i < links.length; i++) {
        var href = links[i].href;
        if (href.indexOf('eating-out') > -1 && urls.indexOf(href) === -1) {
          urls.push(href);
        }
      }
      return JSON.stringify(urls);
    })()
  ")$result$value

  tryCatch(
    jsonlite::fromJSON(links_json),
    error = function(e) character()
  )
}

#' Extract venue name from GFG page
#'
#' The venue name is usually the 2nd keyword in JSON-LD, but sometimes
#' a tag like "Accepts bookings" lands there instead. Uses a known-tag
#' blocklist to skip past non-name keywords, then falls back to the
#' HTML \code{<title>} tag.
#' @noRd
gfg_extract_venue_name <- function(kw_list, page) {
  # Known GFG tags that are NOT venue names
  gfg_tags <- c(
    # Features
    "accepts bookings", "gluten-free options", "good for groups",
    "licensed", "unlicensed", "pet friendly", "pet-friendly",
    "wheelchair accessible", "wheelchair access", "kid friendly",
    "byo", "outdoor dining", "takeaway", "delivery", "dine-in",
    "private dining", "functions", "catering", "live music",
    "waterfront", "rooftop", "courtyard", "fireplace",
    "walk-ins only", "long lunch", "open fire", "events",
    "pre- or post-theatre", "date night", "vegetarian-friendly",
    "vegan-friendly", "vegetarian", "vegan", "vegetarian or vegan",
    "great or interesting view", "set menu",
    "lunch specials", "green & eco focus", "separate bar",
    "cooking classes", "cellar door", "show cooking",
    # Occasions / categories
    "special occasion", "family-friendly", "budget friendly",
    "good for solo diners", "good for business lunch", "brunch",
    "breakfast", "lunch", "dinner", "supper", "afternoon tea",
    "high tea", "morning tea",
    "prix fixe", "set menu", "tasting menu", "degustation",
    "private dining room", "chef's table", "counter dining",
    "critics' pick", "for subscribers", "casual dining", "fine dining",
    # Types
    "restaurant", "cafe", "bar", "pub", "bakery", "deli", "bistro",
    "wine bar", "bottle shop", "food shop", "food truck", "food court",
    "cake shop", "grocer", "pizzeria", "cellar door", "sandwich shop",
    "sandwiches", "noodle bar", "dumpling bar", "juice bar",
    # Meta / platform
    "reviews", "good food app", "good weekend", "suburb guides",
    "new south wales", "victoria", "queensland",
    "south australia", "western australia", "tasmania",
    "northern territory", "australian capital territory",
    # Cities
    "sydney", "melbourne", "canberra", "brisbane", "adelaide",
    "perth", "hobart", "darwin",
    # Cuisines (from shared CUISINE_NAMES in utils.R)
    CUISINE_NAMES
  )

  # Strategy 1: First non-tag keyword after suburb (position 2+)
  # Normalize curly apostrophes for comparison
  if (length(kw_list) >= 2) {
    for (kw in kw_list[-1]) {
      kw_norm <- tolower(kw) |> gsub("\u2019", "'", x = _)
      if (!kw_norm %in% gfg_tags && !grepl("^OOH", kw, ignore.case = TRUE)) {
        return(kw)
      }
    }
  }

  # Strategy 2: HTML <title> tag
  title <- rvest::html_element(page, "title") |> rvest::html_text2()
  if (!is.na(title) && nchar(title) > 0) {
    # "Venue Name review: ..." or "Venue Name at Suburb ..."
    name <- stringr::str_extract(title, "^(.+?)(?=\\s+review[:\\s])")
    if (!is.na(name)) return(stringr::str_squish(name))
    # "Venue Name, Suburb - SMH..."
    name <- stringr::str_extract(title, "^([^,]+)(?=,)")
    if (!is.na(name)) return(stringr::str_squish(name))
    # "Venue Name at/in Suburb ..."
    name <- stringr::str_extract(title, "^(.+?)(?=\\s+(?:at|in)\\s)")
    if (!is.na(name)) return(stringr::str_squish(name))
  }

  NA_character_
}

#' Build Good Food reviews URL for a city
#'
#' Sydney reviews are on smh.com.au, Melbourne on theage.com.au.
#' @noRd
gfg_reviews_url <- function(city) {
  switch(city,
    sydney    = "https://www.smh.com.au/goodfood/reviews",
    melbourne = "https://www.theage.com.au/goodfood/reviews"
  )
}

#' Fetch structured data from a single Good Food Guide detail page
#'
#' Extracts venue name, score, cuisine, price, address, and suburb from
#' JSON-LD keywords and page text.
#' @noRd
gfg_fetch_detail <- function(url, use_cache = FALSE) {
  html_str <- tryCatch(
    cached_fetch(url, use_cache = use_cache),
    error = function(e) NULL
  )
  if (is.null(html_str)) return(NULL)

  page <- rvest::read_html(html_str)
  page_text <- rvest::html_text2(page)

  # Extract JSON-LD
  json_ld_text <- rvest::html_elements(page, "script[type='application/ld+json']") |>
    rvest::html_text()

  article_data <- NULL
  breadcrumb_data <- NULL
  for (jt in json_ld_text) {
    parsed <- tryCatch(jsonlite::fromJSON(jt, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(parsed)) next
    if (identical(parsed$`@type`, "NewsArticle")) article_data <- parsed
    if (identical(parsed$`@type`, "BreadcrumbList")) breadcrumb_data <- parsed
  }

  if (is.null(article_data)) return(NULL)

  # Parse keywords for venue name, cuisine, suburb, and other metadata
  keywords <- article_data$keywords %||% ""
  kw_list <- stringr::str_split(keywords, ",\\s*")[[1]] |>
    stringr::str_squish()

  description <- article_data$description %||% NA_character_

  # Suburb from breadcrumb (last item) or first keyword — extract before name
  suburb <- NA_character_
  if (!is.null(breadcrumb_data)) {
    items <- breadcrumb_data$itemListElement
    if (is.list(items) && length(items) >= 3) {
      suburb <- items[[length(items)]]$name %||% NA_character_
    }
  }
  if (is.na(suburb) && length(kw_list) >= 1) {
    suburb <- kw_list[1]
  }

  # Extract venue name (2nd keyword, or from <title> tag)
  name <- gfg_extract_venue_name(kw_list, page)

  # Review date from JSON-LD datePublished
  review_date <- tryCatch({
    dp <- article_data$datePublished
    if (!is.null(dp)) as.Date(dp) else NA_Date_
  }, error = function(e) NA_Date_)
  # Convert to character for tibble consistency
  review_date <- as.character(review_date)

  # Score with .5 precision — pattern: "15.5\n/20" or "15\n/20"
  score_match <- stringr::str_extract(page_text, "\\d{1,2}(?:\\.5)?(?=\\s*/\\s*20)")
  rating <- if (!is.na(score_match)) as.double(score_match) else NA_real_

  # Cuisine from aria-label "Cuisine: ..." span
  cuisine_el <- rvest::html_element(page, "span[aria-label^='Cuisine:']")
  cuisine <- if (!is.na(cuisine_el)) {
    rvest::html_attr(cuisine_el, "aria-label") |>
      stringr::str_remove("^Cuisine:\\s*") |>
      stringr::str_squish()
  } else {
    NA_character_
  }

  # Price from the active dollar-sign span (role="img", NOT aria-hidden)
  price_el <- rvest::html_element(page, "span[role='img'][aria-label*='cost']")
  price_str <- if (!is.na(price_el)) rvest::html_text2(price_el) else NA_character_
  price_range <- if (!is.na(price_str)) nchar(price_str) else NA_integer_

  # Cost range from aria-label (e.g. "$200-$350")
  cost_range <- if (!is.na(price_el)) {
    stringr::str_extract(
      rvest::html_attr(price_el, "aria-label"),
      "\\$[\\d,]+-\\$[\\d,]+"
    )
  } else {
    NA_character_
  }

  # Fallback: extract cuisine from keywords
  if (is.na(cuisine) && length(kw_list) >= 3) {
    found <- kw_list[tolower(kw_list) %in% CUISINE_NAMES]
    if (length(found) > 0) cuisine <- paste(found, collapse = ", ")
  }

  # Chef hat count from aria-label (e.g. "1 Good Food hat")
  hat_el <- rvest::html_element(page, "span[aria-label*='Good Food hat']")
  hats <- if (!is.na(hat_el)) {
    as.integer(stringr::str_extract(rvest::html_attr(hat_el, "aria-label"), "\\d+"))
  } else {
    NA_integer_
  }

  # Address from Google Maps link (much more reliable than page text regex)
  address_el <- rvest::html_element(page, "a[href*='google.com/maps']")
  address <- if (!is.na(address_el)) {
    rvest::html_text2(address_el) |> stringr::str_squish()
  } else {
    NA_character_
  }

  # Phone number
  phone_el <- rvest::html_element(page, "a[href^='tel:']")
  phone <- if (!is.na(phone_el)) {
    rvest::html_text2(phone_el) |> stringr::str_squish()
  } else {
    NA_character_
  }

  # Website URL — extract from page text after "Website" label
  website <- stringr::str_extract(page_text, "(?<=Website\\s{0,5}\n{1,3})(https?://[^\\s\\n]+)") %||%
    NA_character_

  tibble::tibble(
    name         = name %||% NA_character_,
    suburb       = suburb,
    address      = address,
    cuisine      = cuisine %||% NA_character_,
    category     = "Restaurant",
    description  = description,
    price_range  = as.integer(price_range),
    cost_range   = cost_range %||% NA_character_,
    rating       = rating,
    rating_scale = if (!is.na(rating)) "gfg_score_20" else NA_character_,
    hats         = hats,
    review_date  = review_date %||% NA_character_,
    phone        = phone,
    website      = website,
    latitude     = NA_real_,
    longitude    = NA_real_,
    url          = url
  )
}
