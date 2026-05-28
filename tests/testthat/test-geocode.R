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

test_that("build_geocode_query uses US label when country='US'", {
  expect_equal(
    build_geocode_query("Tartine", "San Francisco", country = "US"),
    "Tartine San Francisco United States"
  )
})

test_that("build_geocode_query drops country label when unknown", {
  expect_equal(
    build_geocode_query("Foo", "Bar", country = "XX"),
    "Foo Bar"
  )
  expect_equal(
    build_geocode_query("Foo", "Bar", country = NULL),
    "Foo Bar"
  )
})

test_that("build_geocode_query appends state when address is missing and city supplied", {
  # No address means the postcode can't disambiguate small towns, so we
  # add the state label as a hint.
  expect_equal(
    build_geocode_query("Norma", "Albury", country = "AU", city = "sydney"),
    "Norma Albury NSW Australia"
  )
})

test_that("build_geocode_query omits state when address is present", {
  # Postcode in the address makes the state hint redundant.
  expect_equal(
    build_geocode_query("Pipit", "Pottsville",
                        address = "4/8 Coronation Avenue, Pottsville, 2489",
                        country = "AU", city = "sydney"),
    "Pipit 4/8 Coronation Avenue, Pottsville, 2489 Pottsville Australia"
  )
})

test_that("build_geocode_query omits state when city has no registered state", {
  # city_state() returns NA for unknown cities; the query falls back
  # to the pre-state behaviour cleanly.
  expect_equal(
    build_geocode_query("Foo", "Bar", country = "AU", city = "unknown-city"),
    "Foo Bar Australia"
  )
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
