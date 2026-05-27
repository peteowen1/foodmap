# Tests for Bon Appétit scraper helpers in R/scrape_bonappetit.R

# --- bonappetit_title_case --------------------------------------------------

test_that("ALL CAPS names are title-cased", {
  expect_equal(bonappetit_title_case("BAAN MAE"), "Baan Mae")
  expect_equal(bonappetit_title_case("HA'S SNACK BAR"), "Ha's Snack Bar")
})

test_that("already mixed-case names pass through", {
  expect_equal(bonappetit_title_case("Bar Bête"), "Bar Bête")
  expect_equal(bonappetit_title_case("My Loup"), "My Loup")
})

test_that("NA / empty pass through", {
  expect_true(is.na(bonappetit_title_case(NA_character_)))
  expect_equal(bonappetit_title_case(""), "")
})


# --- bonappetit_parse_strong_pairs ------------------------------------------

test_that("venue + city strong-pair pattern parses correctly", {
  html <- paste0(
    "<strong>BAAN MAE</strong>",
    "<strong>WASHINGTON, DC |</strong>",
    "<strong>DOGON</strong>",
    "<strong>WASHINGTON, DC |</strong>",
    "<strong>HA'S SNACK BAR</strong>",
    "<strong>NEW YORK CITY |</strong>"
  )
  rows <- bonappetit_parse_strong_pairs(html, "u")
  expect_length(rows, 3)
  combined <- dplyr::bind_rows(rows)
  expect_equal(combined$name, c("Baan Mae", "Dogon", "Ha's Snack Bar"))
  expect_equal(combined$suburb, c("WASHINGTON, DC", "WASHINGTON, DC", "NEW YORK CITY"))
})

test_that("strong blocks without city pair are skipped", {
  # Pull-quote strong blocks (no following CITY | label) should not be
  # mistaken for venues.
  html <- paste0(
    "<strong>This is a pull quote, not a venue</strong>",
    "<strong>BAAN MAE</strong>",
    "<strong>WASHINGTON, DC |</strong>"
  )
  rows <- bonappetit_parse_strong_pairs(html, "u")
  expect_length(rows, 1)
  expect_equal(rows[[1]]$name, "Baan Mae")
})


# --- bonappetit_parse_bare_h2 (2024 fallback) -------------------------------

test_that("bare H2 venues are extracted in fallback mode", {
  html <- paste0("<h2>Agni</h2><h2>Akahoshi Ramen</h2><h2>Bar del Monte</h2>")
  rows <- bonappetit_parse_bare_h2(html, "u")
  expect_length(rows, 3)
  expect_equal(rows[[1]]$name, "Agni")
  expect_true(is.na(rows[[1]]$suburb))
})

test_that("nav / TOC H2s are excluded", {
  html <- "<h2>Search by Region</h2><h2>Best New Restaurants 2024</h2><h2>Agni</h2>"
  rows <- bonappetit_parse_bare_h2(html, "u")
  expect_length(rows, 1)
  expect_equal(rows[[1]]$name, "Agni")
})

test_that("duplicate H2s (TOC mirror) collapse", {
  html <- "<h2>Agni</h2><h2>Agni</h2><h2>Bar</h2>"
  rows <- bonappetit_parse_bare_h2(html, "u")
  expect_length(rows, 2)
})


# --- bonappetit_city_labels (city filter helper) -----------------------------

test_that("city labels cover known cities", {
  expect_true("New York" %in% bonappetit_city_labels("new-york"))
  expect_true("Brooklyn" %in% bonappetit_city_labels("new-york"))
  expect_true("Los Angeles" %in% bonappetit_city_labels("los-angeles"))
  expect_true("Santa Monica" %in% bonappetit_city_labels("los-angeles"))
  expect_true("San Francisco" %in% bonappetit_city_labels("san-francisco"))
  expect_true("Honolulu" %in% bonappetit_city_labels("honolulu"))
})

test_that("unknown city aborts", {
  expect_error(bonappetit_city_labels("atlantis"))
})
