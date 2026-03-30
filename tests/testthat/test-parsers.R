# Parser tests using HTML fixtures
# These verify parsing logic without hitting live websites

fixture_path <- function(name) {
  testthat::test_path("fixtures", name)
}

# --- Time Out ---
test_that("timeout_parse_card extracts data from fixture", {
  card <- rvest::read_html(fixture_path("timeout_card.html")) |>
    rvest::html_element("article.tile")
  result <- timeout_parse_card(card, "sydney")

  expect_s3_class(result, "tbl_df")
  expect_equal(result$name, "Quay")
  expect_equal(result$price_range, 3L)
  expect_equal(result$rating, 5)
  expect_equal(result$cuisine, "Modern Australian")
  expect_equal(result$suburb, "The Rocks")
  expect_true(grepl("Peter Gilmore", result$description))
  expect_true(grepl("Overseas Passenger Terminal", result$address))
  expect_equal(result$url, "https://www.timeout.com/sydney/restaurants/quay")
})

# --- Gourmet Traveller ---
test_that("gt_parse_item extracts data from fixture", {
  item <- rvest::read_html(fixture_path("gourmet_traveller_item.html")) |>
    rvest::html_element("div.wp-block-xwp-listicle-item")
  result <- gt_parse_item(item, "sydney")

  expect_s3_class(result, "tbl_df")
  expect_equal(result$name, "Aria")
  expect_equal(result$price_range, 4L)
  expect_equal(result$cuisine, "Modern Australian")
  expect_true(grepl("Macquarie", result$address))
  expect_equal(result$suburb, "Sydney")
  expect_true(grepl("Matt Moran", result$description))
})

# --- Urban List ---
test_that("urban_list_parse_content extracts restaurants from fixture", {
  content <- rvest::read_html(fixture_path("urban_list_content.html")) |>
    rvest::html_element(".editable-content")
  result <- urban_list_parse_content(content, "sydney")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$name[1], "Fratelli Paradiso")
  expect_equal(result$name[2], "Ormeggio")
  expect_equal(result$suburb[1], "Potts Point")
  expect_equal(result$suburb[2], "Mosman")
  # Image credit paragraphs should be excluded from descriptions
  expect_false(grepl("Image credit", result$description[2]))
})

# --- AGFG table row ---
test_that("agfg_parse_row extracts data from fixture", {
  tr <- rvest::read_html(fixture_path("agfg_table.html")) |>
    rvest::html_element("tr")
  result <- agfg_parse_row(tr)

  expect_s3_class(result, "tbl_df")
  expect_equal(result$name, "Quay")
  expect_equal(result$suburb, "The Rocks")
  expect_equal(result$cuisine, "Modern Australian")
  expect_equal(result$rating, 19)
  expect_equal(result$url, "https://www.agfg.com.au/restaurant/quay-the-rocks")
})

# --- AGFG JSON-LD detail parsing ---
test_that("AGFG JSON-LD fixture parses coordinates and address", {
  page <- rvest::read_html(fixture_path("agfg_jsonld.html"))
  json_ld <- rvest::html_element(page, "script[type='application/ld+json']") |>
    rvest::html_text()
  data <- jsonlite::fromJSON(json_ld)

  geo <- data$geo %||% list()
  lat <- as.numeric(geo$latitude %||% NA_real_)
  lon <- as.numeric(geo$longitude %||% NA_real_)

  expect_equal(lat, -33.8568)
  expect_equal(lon, 151.2100)

  address <- paste(
    c(data$address$streetAddress, data$address$addressLocality,
      data$address$addressRegion, data$address$postalCode),
    collapse = ", "
  ) |> stringr::str_squish()
  expect_true(grepl("The Rocks", address))
  expect_true(grepl("NSW", address))
})
