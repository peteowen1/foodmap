# Tests for Eater scraper helpers in R/scrape_eater.R

# --- eater_suburb_from_address ----------------------------------------------

test_that("standard SF address yields city as suburb", {
  expect_equal(
    eater_suburb_from_address("2700 Jones St, San Francisco, CA, 94133, US"),
    "San Francisco"
  )
})

test_that("neighborhood-prefixed address yields neighborhood", {
  expect_equal(
    eater_suburb_from_address("123 Main St, Presidio, CA, 94129, US"),
    "Presidio"
  )
})

test_that("United States variants are stripped", {
  expect_equal(
    eater_suburb_from_address("1 Market St, Oakland, CA, 94612, United States"),
    "Oakland"
  )
})

test_that("missing or empty address returns NA", {
  expect_true(is.na(eater_suburb_from_address(NA_character_)))
  expect_true(is.na(eater_suburb_from_address("")))
})

test_that("address with only state/postcode/country returns NA", {
  expect_true(is.na(eater_suburb_from_address("CA, 94133, US")))
})


# --- eater_parse_guide ------------------------------------------------------

test_that("inline location JSON is extracted into rows", {
  html <- paste0(
    '<html>... ',
    '"location":{"latitude":37.78,"longitude":-122.41},"name":"Tartine Bakery"',
    '... blah ... "venue":{"address":"600 Guerrero St, San Francisco, CA, 94110, US"}',
    ' ... more html ...',
    '"location":{"latitude":37.77,"longitude":-122.42},"name":"State Bird"',
    '... "venue":{"address":"1529 Fillmore St, San Francisco, CA, 94115, US"}',
    '</html>'
  )
  result <- eater_parse_guide(html)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$name[1], "Tartine Bakery")
  expect_equal(result$name[2], "State Bird")
  expect_equal(result$latitude[1], 37.78)
  expect_equal(result$longitude[2], -122.42)
  expect_true(grepl("Guerrero", result$address[1]))
})

test_that("empty html returns NULL not an error", {
  expect_null(eater_parse_guide(""))
  expect_null(eater_parse_guide("<html>no venues</html>"))
})
