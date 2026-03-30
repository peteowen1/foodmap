# Tests for geocoding helpers in R/geocode_restaurants.R

test_that("build_geocode_query combines name and suburb", {
  expect_equal(build_geocode_query("Aria", "Sydney"), "Aria Sydney Australia")
})

test_that("build_geocode_query handles NA suburb", {
  expect_equal(build_geocode_query("Aria", NA_character_), "Aria Australia")
})

test_that("build_geocode_query handles NA name", {
  expect_equal(build_geocode_query(NA_character_, "Sydney"), "Sydney Australia")
})

test_that("build_geocode_query handles empty strings", {
  expect_equal(build_geocode_query("Aria", ""), "Aria Australia")
})

test_that("ensure_geocode_cols adds missing columns", {
  df <- tibble::tibble(name = "test", latitude = 1.0)
  result <- ensure_geocode_cols(df)
  expect_true("formatted_address" %in% names(result))
  expect_true("place_id" %in% names(result))
})

test_that("ensure_geocode_cols preserves existing columns", {
  df <- tibble::tibble(
    name = "test",
    latitude = 1.0,
    formatted_address = "123 Main St",
    place_id = "abc"
  )
  result <- ensure_geocode_cols(df)
  expect_equal(result$formatted_address, "123 Main St")
  expect_equal(result$place_id, "abc")
})

test_that("resolve_api_key rejects missing key", {
  withr::with_envvar(c(GOOGLE_PLACES_API_KEY = ""), {
    expect_error(resolve_api_key(NULL), "No Google Places API key")
  })
})

test_that("resolve_api_key accepts explicit key", {
  expect_equal(resolve_api_key("my-key"), "my-key")
})

test_that("resolve_api_key reads env var", {
  withr::with_envvar(c(GOOGLE_PLACES_API_KEY = "env-key"), {
    expect_equal(resolve_api_key(NULL), "env-key")
  })
})
