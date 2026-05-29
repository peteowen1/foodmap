# Tests for the OSM Nominatim backend in R/geocode_nominatim.R
#
# The HTTP call itself is not exercised against the live API - we test the
# parameter assembly, result picker, and address-detail mapping in isolation
# so the suite stays offline and deterministic.

test_that("build_nominatim_params includes countrycodes when country known", {
  p <- build_nominatim_params("Tartine SF", country = "US")
  expect_equal(p$q, "Tartine SF")
  expect_equal(p$format, "jsonv2")
  expect_equal(p$countrycodes, "us")
  expect_equal(p$addressdetails, 1L)
})

test_that("build_nominatim_params drops countrycodes when country missing", {
  # Empty/NA country must NOT send `countrycodes=` - Nominatim would
  # interpret that as "anywhere" silently.
  p1 <- build_nominatim_params("Foo", country = NA_character_)
  p2 <- build_nominatim_params("Foo", country = NULL)
  expect_null(p1$countrycodes)
  expect_null(p2$countrycodes)
})

test_that("build_nominatim_params sets viewbox + bounded for known city", {
  p <- build_nominatim_params("Aria", country = "AU", city = "sydney")
  expect_true(!is.null(p$viewbox))
  expect_equal(p$bounded, 1L)
})

test_that("build_nominatim_params sets viewbox but not bounded for country only", {
  # Country bbox is too loose to use as a hard filter - we re-check in the
  # picker instead. bounded=1 only when we have a tight city box.
  p <- build_nominatim_params("Aria", country = "AU", city = NULL)
  expect_true(!is.null(p$viewbox))
  expect_null(p$bounded)
})

test_that("pick_nominatim_result prefers amenity=restaurant over street match", {
  # Same in-bbox, but the venue node should win over the street that
  # happens to share the address line.
  results <- list(
    list(lat = "-33.86", lon = "151.21", class = "highway", type = "primary",
         display_name = "Macquarie Street", osm_type = "way", osm_id = 1),
    list(lat = "-33.86", lon = "151.21", class = "amenity", type = "restaurant",
         display_name = "Aria, 1 Macquarie Street",
         osm_type = "node", osm_id = 2,
         address = list(neighbourhood = "Circular Quay"))
  )
  pick <- pick_nominatim_result(results, country = "AU", city = "sydney")
  expect_equal(pick$osm_id, 2)
})

test_that("pick_nominatim_result falls back to first in-bbox when no amenity", {
  results <- list(
    list(lat = "-33.86", lon = "151.21", class = "building", type = "yes",
         display_name = "Some building", osm_type = "way", osm_id = 99)
  )
  pick <- pick_nominatim_result(results, country = "AU", city = "sydney")
  expect_equal(pick$osm_id, 99)
})

test_that("pick_nominatim_result rejects out-of-region hits", {
  # An NYC venue with an AU-named match shouldn't survive the region filter.
  results <- list(
    list(lat = "40.71", lon = "-74.00", class = "amenity", type = "restaurant",
         display_name = "Sydney Pizza, NYC", osm_type = "node", osm_id = 5)
  )
  expect_null(pick_nominatim_result(results, country = "AU", city = "sydney"))
})

test_that("pick_nominatim_result returns NULL on empty input", {
  expect_null(pick_nominatim_result(list(), country = "AU", city = "sydney"))
})

test_that("neighborhood_from_nominatim prefers neighbourhood > quarter > city_district", {
  expect_equal(
    neighborhood_from_nominatim(list(
      neighbourhood = "Hayes Valley",
      city_district = "Western Addition"
    )),
    "Hayes Valley"
  )
  expect_equal(
    neighborhood_from_nominatim(list(
      quarter = "SoMa",
      city_district = "South of Market"
    )),
    "SoMa"
  )
})

test_that("neighborhood_from_nominatim returns empty sentinel when address missing", {
  # Empty string (not NA) is the "tried, none found" sentinel - mirrors the
  # Google backend so migrate_neighborhoods can't re-pick these rows.
  expect_equal(neighborhood_from_nominatim(NULL), "")
  expect_equal(neighborhood_from_nominatim(list()), "")
  expect_equal(neighborhood_from_nominatim(list(country = "Australia")), "")
})

test_that("neighborhood_from_nominatim skips suburb (handled as its own column)", {
  # `suburb` lives on the restaurant tibble already; copying it into
  # `neighborhood` would render duplicate text in map popups.
  expect_equal(
    neighborhood_from_nominatim(list(suburb = "Newtown")),
    ""
  )
})

test_that("nominatim_place_id composes type/id and handles missing parts", {
  expect_equal(
    nominatim_place_id(list(osm_type = "node", osm_id = 12345)),
    "node/12345"
  )
  expect_equal(nominatim_place_id(list(osm_id = 12345)), NA_character_)
  expect_equal(nominatim_place_id(list(osm_type = "node")), NA_character_)
})

test_that("geocode_restaurants(provider='osm') skips Google API key resolution", {
  # The whole point of the OSM path: runnable without GOOGLE_PLACES_API_KEY.
  # We hit the cache-only branch (n_todo == 0) so no network call fires.
  skip_if_not_installed("withr")
  cache_dir <- withr::local_tempdir()
  cache_path <- file.path(cache_dir, "cache.csv")
  utils::write.csv(
    data.frame(
      name = "Aria", suburb = "Sydney",
      latitude = -33.86, longitude = 151.21,
      formatted_address = "1 Macquarie St", place_id = "node/1",
      neighborhood = "Circular Quay",
      stringsAsFactors = FALSE
    ),
    cache_path, row.names = FALSE
  )
  restaurants <- tibble::tibble(
    name = "Aria", suburb = "Sydney",
    address = NA_character_,
    latitude = NA_real_, longitude = NA_real_
  )
  withr::with_envvar(c(GOOGLE_PLACES_API_KEY = ""), {
    # Would abort with "No Google Places API key" if the OSM path tried to
    # resolve the key. All rows resolve from cache so the loop is skipped.
    expect_no_error(
      suppressMessages(geocode_restaurants(
        restaurants, cache_path = cache_path,
        country = "AU", city = "sydney", provider = "osm"
      ))
    )
  })
})

test_that("nominatim_user_agent identifies the application", {
  # Nominatim bans anonymous clients - the UA must be present and identifying.
  ua <- nominatim_user_agent()
  expect_true(nzchar(ua))
  expect_match(ua, "foodmap", fixed = TRUE)
})
