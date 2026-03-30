#' Scrape the Broadsheet Hotlist
#'
#' Fetches venue data from the Broadsheet Hotlist for a given city.
#'
#' @param city Character. One of "sydney", "melbourne", "brisbane",
#'   "adelaide", "perth", "hobart". Default `"sydney"`.
#' @param use_chromote Logical. If `TRUE`, use a headless Chrome browser
#'   via chromote to render the page fully (loads all venues). If `FALSE`
#'   (default), parse the static HTML only. Falls back to chromote
#'   automatically if static parsing finds no venues and chromote is available.
#'
#' @return A tibble with columns: name, suburb, address, cuisine, category,
#'   description, price_range, latitude, longitude, url.
#' @export
scrape_broadsheet <- function(city = "sydney", use_chromote = FALSE) {

  broadsheet_url(city)  # validates city
  cli::cli_h1("Scraping Broadsheet Hotlist: {city}")

  if (use_chromote) {
    url <- broadsheet_url(city)
    return(scrape_with_chromote(url, city))
  }

  # --- Strategy 1: Direct API (most reliable, gets all pages) ---
  venues <- fetch_venues_api(city)

  # --- Strategy 2: Static HTML / RSC extraction ---
  if (length(venues) == 0) {
    cli::cli_alert_info("API approach failed, trying static HTML extraction...")
    url <- broadsheet_url(city)
    cli::cli_alert_info("Fetching {.url {url}}")
    resp <- httr2::request(url) |>
      httr2::req_headers(`User-Agent` = user_agent_string()) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform()

    html_text <- httr2::resp_body_string(resp)
    venues <- extract_venues_from_rsc(html_text)
  }

  # --- Strategy 3: Chromote headless browser ---
  if (length(venues) == 0) {
    cli::cli_warn(c(
      "No venues found via API or static HTML.",
      "i" = "The page may require JavaScript rendering."
    ))
    if (rlang::is_installed("chromote")) {
      cli::cli_alert_info("Falling back to chromote headless browser...")
      url <- broadsheet_url(city)
      return(scrape_with_chromote(url, city))
    } else {
      cli::cli_abort(c(
        "Could not extract venue data.",
        "i" = "Install {.pkg chromote} for headless browser fallback: {.code install.packages('chromote')}"
      ))
    }
  }

  result <- venues_to_tibble(venues, city)
  cli::cli_alert_success("Found {nrow(result)} venue{?s}")
  result
}

#' Fetch all venues from Broadsheet's internal API with pagination
#' @noRd
fetch_venues_api <- function(city) {
  base_url <- "https://frontend-next.broadsheet.com.au/api/hotlist"
  seed <- as.integer(Sys.time())  # random seed for consistent ordering across pages
  all_venues <- list()
  page <- 1L
  total_pages <- 1L  # updated after first response


  cli::cli_alert_info("Fetching venues from Broadsheet API...")

  repeat {
    resp <- tryCatch(
      httr2::request(base_url) |>
        httr2::req_url_path_append(city) |>
        httr2::req_url_query(order = "random", page = page, seed = seed) |>
        httr2::req_headers(
          `User-Agent` = user_agent_string(),
          `Content-Type` = "application/json"
        ) |>
        httr2::req_body_json(list(filters = list())) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform(),
      error = function(e) {
        cli::cli_warn("API request failed on page {page}: {conditionMessage(e)}")
        NULL
      }
    )

    if (is.null(resp)) break

    data <- tryCatch(
      httr2::resp_body_json(resp),
      error = function(e) NULL
    )

    if (is.null(data) || length(data$venues) == 0) break

    all_venues <- c(all_venues, data$venues)

    # Update total pages from pagination metadata
    pagination <- data$pagination
    tp <- pagination$totalPages
    if (is.numeric(tp) && length(tp) == 1 && tp >= 1) {
      total_pages <- as.integer(tp)
    }

    cli::cli_alert_info(
      "Page {page}/{total_pages}: {length(data$venues)} venues"
    )

    if (page >= total_pages) break
    page <- page + 1L
    Sys.sleep(RATE_LIMIT_SECS)
  }

  if (length(all_venues) > 0) {
    cli::cli_alert_success(
      "Fetched {length(all_venues)} venue{?s} from API ({total_pages} page{?s})"
    )
  }

  all_venues
}

#' Extract venue objects from Next.js RSC payload in HTML
#' @noRd
extract_venues_from_rsc <- function(html_text) {
  # Next.js App Router streams data via self.__next_f.push() calls
  # Extract all push payloads
  push_pattern <- 'self\\.__next_f\\.push\\(\\[\\d+,"((?:[^"\\\\]|\\\\.)*)"\\]\\)'
  matches <- stringr::str_match_all(html_text, push_pattern)[[1]]

  if (nrow(matches) == 0) {
    return(list())
  }

  # Concatenate all payload strings (unescape the JS string escapes)
  payloads <- matches[, 2]
  combined <- paste(payloads, collapse = "")
  # Unescape JS string: \" -> ", \\ -> \, \n -> newline, etc.
  combined <- gsub('\\\\"', '"', combined)
  combined <- gsub('\\\\n', '\n', combined)
  combined <- gsub('\\\\t', '\t', combined)
  combined <- gsub('\\\\\\\\', '\\\\', combined)

  # Strategy 1: Find "venues":[ and extract the full array using bracket counting
  # (regex can't handle nested brackets in cuisine[], primary_address[], etc.)
  all_venues <- list()
  venue_starts <- stringr::str_locate_all(combined, '"venues"\\s*:\\s*\\[')[[1]]

  for (row in seq_len(nrow(venue_starts))) {
    arr_start <- venue_starts[row, "end"]  # position of the opening [
    chars <- utf8ToInt(substr(combined, arr_start, nchar(combined)))
    depth <- 0L
    arr_end <- NA_integer_
    for (k in seq_along(chars)) {
      if (chars[k] == 0x5BL) depth <- depth + 1L        # [
      else if (chars[k] == 0x5DL) depth <- depth - 1L   # ]
      if (depth == 0L) { arr_end <- arr_start + k - 1L; break }
    }
    if (is.na(arr_end)) next

    json_str <- substr(combined, arr_start, arr_end)
    parsed <- tryCatch(
      jsonlite::fromJSON(json_str, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.list(parsed)) {
      all_venues <- c(all_venues, parsed)
    }
  }

  # Strategy 2: Fallback — individual objects containing profile_id + title
  if (length(all_venues) == 0) {
    obj_pattern <- '\\{"profile_id"\\s*:\\s*(?:\\d+|null)[^}]*?"title"\\s*:[^}]*?\\}'
    obj_matches <- stringr::str_extract_all(combined, obj_pattern)[[1]]
    for (m in obj_matches) {
      parsed <- tryCatch(
        jsonlite::fromJSON(m, simplifyVector = FALSE),
        error = function(e) NULL
      )
      if (!is.null(parsed) && !is.null(parsed$title)) {
        all_venues <- c(all_venues, list(parsed))
      }
    }
  }


  # Deduplicate by profile_id (keep venues without an ID)
  ids <- purrr::map_int(all_venues, function(v) {
    pid <- v$profile_id
    if (is.null(pid) || is.na(pid)) 0L else as.integer(pid)
  })
  has_id <- ids != 0L
  keep <- !has_id | (!duplicated(ids) & has_id)
  all_venues <- all_venues[keep]

  all_venues
}

#' Map city to Australian state abbreviation
#' @noRd
state_for_city <- function(city) {
  switch(tolower(city),
    sydney    = "NSW",
    melbourne = "VIC",
    brisbane  = "QLD",
    adelaide  = "SA",
    perth     = "WA",
    hobart    = "TAS",
    ""
  )
}

#' Convert raw venue list to a tidy tibble
#' @noRd
venues_to_tibble <- function(venues, city) {
  state <- state_for_city(city)
  purrr::map(venues, function(v) {
    addr <- v$primary_address[[1]] %||% list()
    cuisine_list <- v$cuisine %||% list()
    cuisine_str <- if (length(cuisine_list) > 0) {
      paste(unlist(cuisine_list), collapse = ", ")
    } else {
      NA_character_
    }

    # Build street address from components if available
    street <- paste(
      c(addr$number, addr$street),
      collapse = " "
    )
    full_address <- stringr::str_squish(
      paste(c(street, v$suburb, state), collapse = ", ")
    )
    if (full_address == paste0(", , ", state) || full_address == paste0(", ", state) || full_address == state) {
      full_address <- NA_character_
    }

    tibble::tibble(
      name         = v$title %||% NA_character_,
      suburb       = v$suburb %||% NA_character_,
      address      = full_address,
      cuisine      = cuisine_str,
      category     = v$category %||% NA_character_,
      description  = v$hotlist_blurb %||% NA_character_,
      price_range  = as.integer(v$pricerange %||% NA_integer_),
      rating       = NA_real_,
      rating_scale = NA_character_,
      latitude     = as.numeric(addr$latitude %||% NA_real_),
      longitude    = as.numeric(addr$longitude %||% NA_real_),
      url          = v$url %||% NA_character_
    )
  }) |>
    dplyr::bind_rows()
}

#' Scrape Broadsheet using chromote headless browser
#' @noRd
scrape_with_chromote <- function(url, city) {
  rlang::check_installed("chromote", reason = "for headless browser scraping")
  cli::cli_alert_info("Launching headless Chrome to load full page...")

  b <- chromote::ChromoteSession$new()
  on.exit(b$close(), add = TRUE)

  b$Page$navigate(url)
  # Wait for page to load and render venues
  Sys.sleep(5)

  # Scroll down to trigger lazy-loading of all venues
  cli::cli_alert_info("Scrolling to load all venues...")
  for (i in 1:20) {
    b$Runtime$evaluate("window.scrollTo(0, document.body.scrollHeight)")
    Sys.sleep(1)
  }

  # Extract the full page HTML after rendering
  result <- b$Runtime$evaluate("document.documentElement.outerHTML")
  html_text <- result$result$value

  # Try RSC extraction on the rendered page

  venues <- extract_venues_from_rsc(html_text)

  # Also try extracting from the DOM directly if RSC fails
  if (length(venues) == 0) {
    cli::cli_alert_info("Trying DOM-based extraction...")
    venues <- extract_venues_from_dom(b)
  }

  if (length(venues) == 0) {
    cli::cli_abort("Could not extract venue data even with chromote.")
  }

  result <- venues_to_tibble(venues, city)
  cli::cli_alert_success("Found {nrow(result)} venue{?s} via chromote")
  result
}

#' Extract venue data by evaluating JavaScript in the browser
#' @noRd
extract_venues_from_dom <- function(browser) {
  # Try to extract venue data from React component state or DOM

  js_code <- "
    (function() {
      // Try to find venue elements in the DOM
      var cards = document.querySelectorAll('[data-testid*=\"venue\"], [class*=\"venue\"], [class*=\"card\"]');
      var venues = [];
      cards.forEach(function(card) {
        var name = card.querySelector('h2, h3, [class*=\"title\"]');
        var suburb = card.querySelector('[class*=\"suburb\"], [class*=\"location\"]');
        if (name) {
          venues.push({
            title: name.textContent.trim(),
            suburb: suburb ? suburb.textContent.trim() : null
          });
        }
      });
      return JSON.stringify(venues);
    })()
  "
  result <- browser$Runtime$evaluate(js_code)
  json_str <- result$result$value

  tryCatch({
    parsed <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
    # Filter to only entries that have a title
    purrr::keep(parsed, ~ !is.null(.x$title) && nchar(.x$title) > 0)
  }, error = function(e) list())
}
