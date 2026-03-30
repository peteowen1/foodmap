#' Scrape The Urban List best restaurants
#'
#' Fetches The Urban List best restaurants for a given city.
#' Content is server-rendered; uses static HTML parsing via rvest.
#'
#' @param city Character. One of "sydney", "melbourne". Default `"sydney"`.
#'
#' @return A tibble with columns: name, suburb, address, cuisine, category,
#'   description, price_range, rating, rating_scale, latitude, longitude, url.
#' @export
scrape_urban_list <- function(city = "sydney", use_cache = FALSE) {
  city <- validate_city_source(city, "urban_list")
  cli::cli_h1("Scraping The Urban List: {city}")

  url <- urban_list_url(city)
  cli::cli_alert_info("Fetching {.url {url}}")

  # Urban List may be behind Cloudflare — check status before parsing
  if (use_cache) {
    cached <- cache_get(url)
    if (!is.null(cached)) {
      cli::cli_alert_info("Using cached response for {.url {url}}")
      page <- rvest::read_html(cached)
    }
  }

  if (!exists("page", inherits = FALSE)) {
    resp <- httr2::request(url) |>
      httr2::req_headers(`User-Agent` = user_agent_string()) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) != 200) {
      cli::cli_abort(c(
        "Failed to fetch Urban List page (status {httr2::resp_status(resp)}).",
        "i" = "The page may be behind Cloudflare protection."
      ))
    }

    html <- httr2::resp_body_string(resp)
    if (use_cache) cache_set(url, html)
    page <- rvest::read_html(html)
  }

  content <- rvest::html_element(page, ".editable-content")
  if (is.na(content) || length(content) == 0) {
    cli::cli_abort("Could not find .editable-content on Urban List page.")
  }

  result <- urban_list_parse_content(content, city)

  n_headings <- length(rvest::html_elements(content, "h2, h3"))
  if (n_headings > 0 && nrow(result) < n_headings / 2) {
    cli::cli_warn(
      "Only {nrow(result)} venue{?s} parsed from {n_headings} heading{?s} \u2014 page structure may have changed"
    )
  }

  if (nrow(result) == 0) {
    cli::cli_abort("No restaurants found on Urban List page.")
  }

  cli::cli_alert_success("Found {nrow(result)} venue{?s}")
  result
}

#' Build Urban List URL for a city
#' @noRd
urban_list_url <- function(city) {
  glue::glue("https://www.theurbanlist.com/{city}/a-list/best-restaurants-{city}")
}

#' Parse the .editable-content block into restaurant entries
#'
#' Uses a heuristic: any H2/H3 immediately followed by an H4 is a restaurant
#' entry (name in H2/H3, address in H4). Section headers are H2/H3 NOT
#' followed by H4.
#' @noRd
urban_list_parse_content <- function(content, city) {
  children <- rvest::html_children(content)
  tags <- rvest::html_name(children)

  restaurants <- list()
  i <- 1L


  while (i <= length(children)) {
    tag <- tags[i]

    # Look for H2 or H3 followed by H4 (name + address pattern)
    if (tag %in% c("h2", "h3") && i < length(children) && tags[i + 1] == "h4") {
      name <- rvest::html_text2(children[[i]])
      address <- rvest::html_text2(children[[i + 1]])

      # Collect description from subsequent <p> elements
      desc_parts <- character()
      j <- i + 2L
      while (j <= length(children) && !tags[j] %in% c("h2", "h3")) {
        if (tags[j] == "p") {
          p_text <- rvest::html_text2(children[[j]])
          # Skip image credits
          if (!grepl("^Image credit", p_text, ignore.case = TRUE) &&
              nchar(p_text) > 0) {
            desc_parts <- c(desc_parts, p_text)
          }
        }
        j <- j + 1L
      }

      description <- if (length(desc_parts) > 0) {
        paste(desc_parts, collapse = " ")
      } else {
        NA_character_
      }

      # Extract suburb from address — take the last comma-part that

      # doesn't look like a postcode (all digits) or state abbreviation
      suburb <- if (nchar(address) > 0) {
        parts <- stringr::str_split(address, ",\\s*")[[1]] |>
          stringr::str_squish()
        # Walk backwards to find the suburb (skip postcodes and state codes)
        found <- NA_character_
        for (p in rev(parts)) {
          if (!grepl("^\\d+$", p) && !grepl("^(NSW|VIC|QLD|SA|WA|TAS|ACT|NT)$", p, ignore.case = TRUE)) {
            # Strip trailing postcode from combined "Suburb NSW 2000" style
            found <- stringr::str_remove(p, "\\s+(NSW|VIC|QLD|SA|WA|TAS|ACT|NT)\\s+\\d{4}$")
            break
          }
        }
        found
      } else {
        NA_character_
      }

      restaurants <- c(restaurants, list(tibble::tibble(
        name         = name,
        suburb       = suburb,
        address      = address,
        cuisine      = NA_character_,
        category     = "Restaurant",
        description  = description,
        price_range  = NA_integer_,
        rating       = NA_real_,
        rating_scale = NA_character_,
        latitude     = NA_real_,
        longitude    = NA_real_,
        url          = NA_character_
      )))

      i <- j
    } else {
      i <- i + 1L
    }
  }

  dplyr::bind_rows(restaurants)
}
