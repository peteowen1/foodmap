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

test_that("geocode_restaurants infers country from city when country=NULL", {
  # This is the footgun guard: pass city = "los-angeles" without
  # country, and the self-heal must validate against the US bbox, not
  # the AU default. We don't hit the API here - the cache-apply step
  # with a single in-LA row should leave coords intact when country
  # resolves to "US", and would wipe them under the old AU default.
  skip_if_not_installed("withr")
  cache_dir <- withr::local_tempdir()
  cache_path <- file.path(cache_dir, "cache.csv")
  utils::write.csv(
    data.frame(
      name = "Kusano", suburb = "Culver City",
      latitude = 34.0021592, longitude = -118.3925679,
      formatted_address = "10726 Jefferson Blvd, Culver City, CA 90230",
      place_id = "ChIJtest", neighborhood = "Blanco - Culver Crest",
      stringsAsFactors = FALSE
    ),
    cache_path, row.names = FALSE
  )

  restaurants <- tibble::tibble(
    name = "Kusano", suburb = "Culver City",
    address = NA_character_,
    latitude = NA_real_, longitude = NA_real_
  )

  # Without country: should infer "US" from city and KEEP the coords.
  result <- suppressMessages(geocode_restaurants(
    restaurants, cache_path = cache_path, city = "los-angeles"
  ))
  expect_equal(result$latitude, 34.0021592)
  expect_equal(result$longitude, -118.3925679)
})

test_that("geocode_restaurants leaves country=AU as the no-city default", {
  # When no city is passed (the original AU-only pipeline shape), the
  # implicit default stays "AU" so existing callers don't regress.
  skip_if_not_installed("withr")
  cache_dir <- withr::local_tempdir()
  cache_path <- file.path(cache_dir, "cache.csv")
  utils::write.csv(
    data.frame(
      name = "Aria", suburb = "Sydney",
      latitude = -33.86, longitude = 151.21,
      formatted_address = "1 Macquarie St", place_id = "x",
      neighborhood = "Test Neighborhood",
      stringsAsFactors = FALSE
    ),
    cache_path, row.names = FALSE
  )
  restaurants <- tibble::tibble(
    name = "Aria", suburb = "Sydney",
    address = NA_character_,
    latitude = NA_real_, longitude = NA_real_
  )
  # No city, no country: AU bbox should accept the Sydney coords.
  result <- suppressMessages(geocode_restaurants(
    restaurants, cache_path = cache_path
  ))
  expect_equal(result$latitude, -33.86)
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
