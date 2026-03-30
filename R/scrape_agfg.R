#' Scrape AGFG (Australian Good Food Guide) award-winning restaurants
#'
#' Fetches award-winning restaurant listings from agfg.com.au/awards for the
#' state corresponding to a given city. Parses the initial HTML table, then
#' paginates via AJAX. Optionally scrapes detail pages for coordinates and
#' address via JSON-LD.
#'
#' @param city Character. One of "sydney", "melbourne", "brisbane", "adelaide",
#'   "perth", "hobart", "canberra", "darwin", "gold-coast". Default `"sydney"`.
#' @param fetch_coordinates Logical. If `TRUE` (default), fetch each venue's
#'   detail page to extract lat/lng and address from JSON-LD. Slower but avoids
#'   needing Google Places API geocoding.
#'
#' @return A tibble with columns: name, suburb, address, cuisine, category,
#'   description, price_range, rating, rating_scale, latitude, longitude, url.
#' @export
scrape_agfg <- function(city = "sydney", fetch_coordinates = TRUE) {
  city <- validate_city_source(city, "agfg")
  state <- agfg_city_to_state(city)
  cli::cli_h1("Scraping AGFG Awards: {city} ({state})")

  # Phase 1: Fetch initial page + paginated results
  all_rows <- agfg_fetch_all_listings(state)

  if (length(all_rows) == 0) {
    cli::cli_abort("No award-winning restaurants found on AGFG for {.val {state}}.")
  }

  cli::cli_alert_info("Parsing {length(all_rows)} listing{?s}...")
  result <- purrr::map(all_rows, agfg_parse_row) |>
    dplyr::bind_rows()

  # Phase 2: Fetch detail pages for coordinates and address
  if (fetch_coordinates && nrow(result) > 0) {
    needs_detail <- is.na(result$latitude) & !is.na(result$url)
    n_fetch <- sum(needs_detail)
    if (n_fetch > 0) {
      cli::cli_alert_info("Fetching {n_fetch} detail page{?s} for coordinates...")
      n_detail_ok <- 0L
      details <- purrr::imap(which(needs_detail), function(i, idx) {
        if (idx %% 50 == 0) {
          cli::cli_alert_info("  ...{idx}/{n_fetch}")
        }
        agfg_fetch_detail(result$url[i])
      })
      for (idx in seq_along(details)) {
        row <- which(needs_detail)[idx]
        d <- details[[idx]]
        if (!is.null(d)) {
          n_detail_ok <- n_detail_ok + 1L
          result$latitude[row]  <- d$latitude
          result$longitude[row] <- d$longitude
          if (is.na(result$address[row]) && !is.null(d$address)) {
            result$address[row] <- d$address
          }
          if (is.na(result$price_range[row]) && !is.null(d$price_range)) {
            result$price_range[row] <- d$price_range
          }
        }
      }
      n_detail_fail <- n_fetch - n_detail_ok
      if (n_detail_fail > 0) {
        cli::cli_warn("{n_detail_fail}/{n_fetch} detail page{?s} failed to fetch or parse")
      }
    }
  }

  cli::cli_alert_success("Found {nrow(result)} venue{?s}")
  result
}

#' Map city names to AGFG state codes
#' @noRd
agfg_city_to_state <- function(city) {
  switch(city,
    sydney       = "nsw",
    melbourne    = "vic",
    brisbane     = "qld",
    adelaide     = "sa",
    perth        = "wa",
    hobart       = "tas",
    canberra     = "act",
    darwin       = "nt",
    `gold-coast` = "qld"
  )
}

#' Approximate state center coordinates for AGFG AJAX requests
#' @noRd
agfg_state_coords <- function(state) {
  coords <- list(
    nsw = list(lat = -33.8688, lon = 151.2093),
    vic = list(lat = -37.8136, lon = 144.9631),
    qld = list(lat = -27.4698, lon = 153.0251),
    sa  = list(lat = -34.9285, lon = 138.6007),
    wa  = list(lat = -31.9505, lon = 115.8605),
    tas = list(lat = -42.8821, lon = 147.3272),
    act = list(lat = -35.2809, lon = 149.1300),
    nt  = list(lat = -12.4634, lon = 130.8456)
  )
  coords[[state]]
}

#' Fetch all AGFG award listing rows via initial page + AJAX pagination
#' @noRd
agfg_fetch_all_listings <- function(state) {
  url <- glue::glue("https://www.agfg.com.au/awards/{state}")
  cli::cli_alert_info("Fetching {.url {url}}")

  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = user_agent_string()) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()

  html_text <- httr2::resp_body_string(resp)
  page <- rvest::read_html(html_text)

  # Extract max page from hidden input
  max_page_el <- rvest::html_element(page, "input#max-page")
  max_page <- if (!is.na(max_page_el)) {
    as.integer(rvest::html_attr(max_page_el, "value"))
  } else {
    # Try extracting from JavaScript: var maxPage = N;
    m <- stringr::str_match(html_text, "maxPage\\s*=\\s*(\\d+)")
    if (!is.na(m[1, 2])) as.integer(m[1, 2]) else 1L
  }
  max_page <- max_page %||% 1L

  # Parse initial table rows — awards page uses <tr> with <td> cells
  all_rows <- rvest::html_elements(page, "tr") |>
    purrr::keep(function(tr) {
      # Only keep rows containing a restaurant link
      length(rvest::html_elements(tr, "a[href*='/restaurant/']")) > 0
    }) |>
    as.list()

  cli::cli_alert_info("Page 1/{max_page}: {length(all_rows)} listings")

  # Fetch remaining pages via AJAX
  if (max_page > 1) {
    state_coords <- agfg_state_coords(state)
    for (pg in 2:max_page) {
      Sys.sleep(RATE_LIMIT_SECS)
      body <- agfg_ajax_body(state, state_coords, pg)

      ajax_resp <- tryCatch(
        httr2::request("https://www.agfg.com.au/handlers/MoreListings.ashx") |>
          httr2::req_url_query(pg = pg) |>
          httr2::req_headers(
            `User-Agent` = user_agent_string(),
            `Content-Type` = "application/json; charset=utf-8",
            `X-Requested-With` = "XMLHttpRequest"
          ) |>
          httr2::req_body_json(body) |>
          httr2::req_retry(max_tries = 3) |>
          httr2::req_perform(),
        error = function(e) {
          cli::cli_warn("AJAX page {pg} failed: {conditionMessage(e)}")
          NULL
        }
      )

      if (is.null(ajax_resp)) next

      html_fragment <- httr2::resp_body_string(ajax_resp)

      # Wrap to force HTML parsing (read_html treats short strings as file paths)
      page_rows <- tryCatch({
        rvest::read_html(paste0("<table>", html_fragment, "</table>")) |>
          rvest::html_elements("tr") |>
          purrr::keep(function(tr) {
            length(rvest::html_elements(tr, "a[href*='/restaurant/']")) > 0
          }) |>
          as.list()
      }, error = function(e) {
        cli::cli_warn("Failed to parse AJAX page {pg}: {conditionMessage(e)}")
        list()
      })

      if (length(page_rows) == 0) break

      all_rows <- c(all_rows, page_rows)
      cli::cli_alert_info("Page {pg}/{max_page}: {length(page_rows)} listings")
    }
  }

  all_rows
}

#' Build AJAX request body for AGFG MoreListings endpoint (awards mode)
#' @noRd
agfg_ajax_body <- function(state, state_coords, page) {
  list(
    NearMe = FALSE,
    IsSearch = FALSE,
    IsFiltered = FALSE,
    Query = "",
    QueryRemoved = NULL,
    Lat = state_coords$lat,
    Lon = state_coords$lon,
    Distance = 0L,
    LocationUrl = state,
    SuburbUrl = "",
    TopLocationUrl = state,
    ListingType = "restaurants",
    SubListingType = NULL,
    CategoryUrl = "",
    CategoryGroup = NULL,
    CategoryIds = "",
    CuisineUrl = NULL,
    CuisineTypeIds = "",
    FunctionTypeUrl = NULL,
    FunctionGuests = 0L,
    OpenNowOnly = FALSE,
    AwardsOnly = TRUE,
    ReadersChoiceOnly = FALSE,
    OrderBy = "award",
    Page = page,
    PageSize = 20L
  )
}

#' Parse a single AGFG awards table row into a tibble row
#'
#' Table structure: image | name+link | award icon | suburb | region | cuisine
#' @noRd
agfg_parse_row <- function(tr) {
  tds <- rvest::html_elements(tr, "td")
  td_texts <- rvest::html_text2(tds) |> stringr::str_squish()

  # Name and URL from the restaurant link
  link <- rvest::html_element(tr, "a[href*='/restaurant/']")
  name <- if (!is.na(link)) rvest::html_text2(link) else NA_character_

  detail_url <- if (!is.na(link)) {
    href <- rvest::html_attr(link, "href")
    if (!grepl("^https?://", href)) paste0("https://www.agfg.com.au", href) else href
  } else {
    NA_character_
  }

  # Last 3 tds are always: suburb, region, cuisine
  # (earlier tds contain image alt text and award icons which shift indices)
  n <- length(td_texts)
  suburb  <- if (n >= 3) td_texts[n - 2] else NA_character_
  cuisine <- if (n >= 1) td_texts[n] else NA_character_

  # Filter out empty values
  if (!is.na(suburb) && nchar(suburb) == 0) suburb <- NA_character_
  if (!is.na(cuisine) && nchar(cuisine) == 0) cuisine <- NA_character_

  # Chef hat score from award image filename (e.g., /awards/19.png → 19)
  award_img <- rvest::html_element(tr, "img[src*='/awards/']") |>
    rvest::html_attr("src")
  rating <- if (!is.na(award_img)) {
    as.double(stringr::str_extract(award_img, "\\d+(?=\\.png)"))
  } else {
    NA_real_
  }

  tibble::tibble(
    name         = name %||% NA_character_,
    suburb       = suburb,
    address      = NA_character_,
    cuisine      = cuisine,
    category     = "Restaurant",
    description  = NA_character_,
    price_range  = NA_integer_,
    rating       = rating,
    rating_scale = if (!is.na(rating)) "agfg_hats_19" else NA_character_,
    latitude     = NA_real_,
    longitude    = NA_real_,
    url          = detail_url
  )
}

#' Fetch full details from an AGFG detail page's JSON-LD
#' @noRd
agfg_fetch_detail <- function(url) {
  Sys.sleep(RATE_LIMIT_SECS)
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_headers(`User-Agent` = user_agent_string()) |>
      httr2::req_retry(max_tries = 2) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NULL)

  page <- httr2::resp_body_string(resp) |> rvest::read_html()
  json_ld <- rvest::html_element(page, "script[type='application/ld+json']") |>
    rvest::html_text()

  if (is.na(json_ld)) return(NULL)

  data <- tryCatch(jsonlite::fromJSON(json_ld), error = function(e) NULL)
  if (is.null(data)) return(NULL)

  geo <- data$geo %||% list()
  lat <- as.numeric(geo$latitude %||% NA_real_)
  lon <- as.numeric(geo$longitude %||% NA_real_)

  address <- if (!is.null(data$address)) {
    paste(
      c(data$address$streetAddress, data$address$addressLocality,
        data$address$addressRegion, data$address$postalCode),
      collapse = ", "
    ) |> stringr::str_squish()
  } else {
    NULL
  }

  price_str <- data$priceRange
  price_range <- if (!is.null(price_str) && grepl("^\\$+$", price_str)) {
    nchar(price_str)
  } else {
    NULL
  }

  list(latitude = lat, longitude = lon, address = address, price_range = price_range)
}
