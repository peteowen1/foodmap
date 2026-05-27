#' Scrape HONOLULU Magazine editorial restaurant roundups
#'
#' HONOLULU Magazine is the city's monthly print/digital magazine and
#' publishes a steady cadence of editorial "best of" roundups. This
#' scraper handles two staple articles:
#'
#' - "Best Restaurants in Honolulu" - the rolling editorial picks
#'   maintained by Mari Taketa, Thomas Obungen and Melissa Chang.
#'   Each venue is anchored with `<h1 style="text-align: center;">`.
#' - "Oʻahu's Best New Restaurants of YYYY" - the annual cohort
#'   article that profiles 5-10 new openings. Each venue is anchored
#'   with a bare `<h2>`.
#'
#' The two layouts are similar enough to share a parser:
#' a venue heading followed by one or more `<p>` paragraphs of
#' editorial prose. The scraper auto-detects which heading level
#' anchors venues by counting candidates at each level and picking
#' the level with more than one match (so a single page H1 doesn't
#' falsely register as a venue heading).
#'
#' Hale ʻAina Awards live in a separate scraper
#' (`scrape_hale_aina()`) - that page has a category/rank shape that
#' doesn't fit the venue-heading pattern.
#'
#' @param city Character. Currently `"honolulu"`. Default
#'   `"honolulu"`.
#' @param extra_guides Character vector. Extra article URLs to fetch
#'   on top of the default editorial set. Default `character()`.
#' @param use_cache Logical. Cache responses for 24h. Default `FALSE`.
#'
#' @return A tibble with the standard scraper schema.
#' @export
scrape_honolulu_magazine <- function(city = "honolulu",
                                     extra_guides = character(),
                                     use_cache = FALSE) {
  city <- validate_city_source(city, "honolulu_magazine")
  cli::cli_h1("Scraping HONOLULU Magazine: {city}")

  guides <- unique(c(honolulu_mag_default_guides(city), extra_guides))
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
    honolulu_mag_parse_article(html_str, url)
  })
  results <- purrr::compact(results)
  if (length(results) == 0) {
    cli::cli_abort("No data scraped from any HONOLULU Magazine article.")
  }

  combined <- dplyr::bind_rows(results) |>
    dplyr::distinct(.data$name, .keep_all = TRUE)
  cli::cli_alert_success("Found {nrow(combined)} venue{?s}")
  combined
}


#' Default editorial article URLs per city
#'
#' These are the magazine's evergreen roundups that get refreshed
#' periodically. The annual "Best New Restaurants" slug rolls each
#' year, so when the next cohort lands we'll add the new URL here
#' and let the old one age out via the parser's natural dedup.
#' @noRd
honolulu_mag_default_guides <- function(city) {
  switch(city,
    honolulu = c(
      "https://www.honolulumagazine.com/best-restaurants-in-honolulu/",
      "https://www.honolulumagazine.com/oahu-best-new-restaurants-2025/"
    ),
    cli::cli_abort("No default HONOLULU Magazine guides for {.val {city}}")
  )
}


#' Parse a single HONOLULU Magazine roundup article
#'
#' Locates the venue heading level (H1 for "Best Restaurants in
#' Honolulu", H2 for "Best New Restaurants"), pulls each heading's
#' text as the venue name, and grabs the first few `<p>` blocks that
#' follow as the description.
#' @noRd
honolulu_mag_parse_article <- function(html_str, url) {
  # Restrict to entry-content if available so navigation/sidebar
  # headings don't leak in.
  body <- honolulu_mag_entry_body(html_str)

  # Try H1 first (rolling "Best Restaurants" layout), then H2 (annual
  # "Best New Restaurants" layout). The body of an article has
  # exactly one entry-title H1 from the article frame; venue headings
  # use a distinguishing `style="text-align: center;"` attribute, so
  # we anchor on that. H2 venue headings are bare `<h2>NAME</h2>`.
  h1_venues <- honolulu_mag_extract_headings(
    body, level = 1L,
    attr_filter = "text-align"
  )
  h2_venues <- honolulu_mag_extract_headings(
    body, level = 2L,
    attr_filter = NULL,
    require_bare = TRUE
  )

  venues <- if (length(h1_venues$names) >= 5) {
    h1_venues
  } else if (length(h2_venues$names) >= 3) {
    h2_venues
  } else {
    cli::cli_warn(
      "  Article at {.url {url}} produced no recognised venue headings"
    )
    return(NULL)
  }

  rows <- lapply(seq_along(venues$names), function(i) {
    name <- venues$names[i]
    block_start <- venues$ends[i]
    block_end <- if (i < length(venues$names)) venues$starts[i + 1] else nchar(body)
    block <- substr(body, block_start, block_end)

    description <- honolulu_mag_extract_description(block)

    tibble::tibble(
      name         = name,
      suburb       = NA_character_,
      address      = NA_character_,
      cuisine      = prose_to_cuisine(description),
      category     = "Restaurant",
      description  = description,
      price_range  = NA_integer_,
      rating       = NA_real_,
      rating_scale = NA_character_,
      latitude     = NA_real_,
      longitude    = NA_real_,
      url          = url
    )
  })
  dplyr::bind_rows(rows)
}


#' Slice the article-body region out of a HONOLULU Magazine page
#'
#' HM articles don't wrap the body in a single named div. Instead they
#' open with the article's own `<h1 class="entry-title">...</h1>` and
#' the body runs until either the "Related" / "Most Popular" sidebar
#' H2 or the article-footer block, whichever comes first. Anchoring on
#' those two landmarks keeps sidebar / nav headings out of the heading
#' scanner.
#'
#' Falls back to the full HTML when the entry-title can't be located
#' (the parser's own skip rules then catch non-venue headings).
#' @noRd
honolulu_mag_entry_body <- function(html_str) {
  start <- stringr::str_locate(
    html_str, "<h1[^>]*class=\"entry-title\""
  )[1, "start"]
  if (is.na(start)) return(html_str)
  tail <- substr(html_str, start, nchar(html_str))
  end_match <- stringr::str_locate(tail, "<h2[^>]*>(Related|Most Popular)")
  if (!is.na(end_match[1, "start"])) {
    tail <- substr(tail, 1, end_match[1, "start"] - 1L)
  }
  tail
}


#' Extract venue headings at a given level
#'
#' @param body Character. HTML block to scan.
#' @param level Integer. Heading level (1 or 2).
#' @param attr_filter Optional substring required in the opening tag's
#'   attributes (e.g. `"text-align"` to require centered headings).
#'   `NULL` means no attribute requirement.
#' @param require_bare Logical. When `TRUE`, also require that the
#'   opening tag has no class attribute - this filters out
#'   `<h2 class="widgettitle">` and similar non-content headings.
#'   Default `FALSE`.
#' @return Named list with `names` (character vector of venue names),
#'   `starts` and `ends` (integer positions in `body`).
#' @noRd
honolulu_mag_extract_headings <- function(body, level, attr_filter = NULL,
                                           require_bare = FALSE) {
  tag <- paste0("h", level)
  pattern <- paste0("<", tag, "([^>]*)>([\\s\\S]*?)</", tag, ">")
  matches <- stringr::str_match_all(body, pattern)[[1]]
  positions <- stringr::str_locate_all(body, pattern)[[1]]
  if (nrow(matches) == 0) {
    return(list(names = character(), starts = integer(), ends = integer()))
  }

  keep <- rep(TRUE, nrow(matches))
  attrs <- matches[, 2]
  for (i in seq_len(nrow(matches))) {
    if (!is.null(attr_filter) && !grepl(attr_filter, attrs[i], fixed = TRUE)) {
      keep[i] <- FALSE; next
    }
    if (require_bare && grepl("class=", attrs[i], fixed = TRUE)) {
      keep[i] <- FALSE; next
    }
  }
  matches <- matches[keep, , drop = FALSE]
  positions <- positions[keep, , drop = FALSE]
  if (nrow(matches) == 0) {
    return(list(names = character(), starts = integer(), ends = integer()))
  }

  # vapply preserves the input vector's names by default; we want a
  # plain character vector so downstream indexing isn't affected by
  # accidental name attrs (the original raw HTML, which is loud and
  # misleading in test output).
  names <- unname(vapply(matches[, 3], function(raw) {
    raw |>
      strip_html_tags() |>
      decode_html_entities() |>
      stringr::str_squish()
  }, character(1)))

  # Drop obvious non-venue headings: hours-of-week labels, the article's
  # own subtitle, divider artifacts ("* * *", "____").
  bad <- !nzchar(names) | nchar(names) < 2 | nchar(names) > 80 |
    grepl("^(\\*|_|\u2193|\u2014|\u2013|\\s)+$", names, perl = TRUE) |
    grepl("(?i)\\b(restaurant|magazine|honolulu|sponsored|advertisement|update)\\b",
          names, perl = TRUE) &
    grepl("(?i)\\b(top|best|guide|article|story)\\b", names, perl = TRUE)
  names <- names[!bad]
  positions <- positions[!bad, , drop = FALSE]

  list(
    names  = names,
    starts = positions[, "start"],
    ends   = positions[, "end"]
  )
}


#' First non-trivial paragraph after a venue heading
#'
#' Strip HTML tags, decode entities, squish whitespace, truncate at
#' 500 chars to keep popup payloads light. Returns `NA_character_`
#' when no paragraph longer than 40 characters is found in the block.
#' @noRd
honolulu_mag_extract_description <- function(block) {
  paragraphs <- stringr::str_match_all(block, "<p[^>]*>([\\s\\S]*?)</p>")[[1]]
  if (nrow(paragraphs) == 0) return(NA_character_)
  for (k in seq_len(nrow(paragraphs))) {
    text <- paragraphs[k, 2] |>
      strip_html_tags() |>
      decode_html_entities() |>
      stringr::str_squish()
    if (nchar(text) < 40) next
    if (nchar(text) > 500) text <- paste0(substr(text, 1, 497), "...")
    return(text)
  }
  NA_character_
}
