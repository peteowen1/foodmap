# Tests for apply_manual_excludes(). The function reads from
# inst/extdata/manual_excludes.csv via system.file(), so we can't
# substitute the file directly - but we can test against whatever
# the shipped CSV contains. The shipped Honolulu entries (Av
# Restaurant, Mandalay Restaurant, Cino, Noi Thai Cuisine, Leila) and
# the SF entry (Gozu) act as stable fixtures.

test_that("apply_manual_excludes drops Honolulu Australian-cache-pollution rows", {
  restaurants <- tibble::tibble(
    name = c("Av Restaurant", "Leila", "Real Honolulu Venue"),
    suburb = c(NA_character_, NA_character_, "Waikiki"),
    source = c("thrillist", "hale_aina", "thrillist")
  )
  result <- apply_manual_excludes(restaurants, city = "honolulu")
  expect_equal(result$name, "Real Honolulu Venue")
})

test_that("apply_manual_excludes respects source qualifier", {
  # Av Restaurant is excluded for thrillist source specifically; a
  # different source with the same name should pass through.
  restaurants <- tibble::tibble(
    name = c("Av Restaurant", "Av Restaurant"),
    suburb = c(NA_character_, NA_character_),
    source = c("thrillist", "some_other_source")
  )
  result <- apply_manual_excludes(restaurants, city = "honolulu")
  expect_equal(result$source, "some_other_source")
})

test_that("apply_manual_excludes scopes by pipeline city", {
  # Gozu is only excluded under the san-francisco pipeline. A Sydney
  # pipeline pass over the same data must not apply that exclude.
  restaurants <- tibble::tibble(
    name = c("Gozu", "Other Venue"),
    suburb = c("Embarcadero", "Foo"),
    source = c("eater", "eater")
  )
  result_sf <- apply_manual_excludes(restaurants, city = "san-francisco")
  expect_equal(result_sf$name, "Other Venue")

  result_syd <- apply_manual_excludes(restaurants, city = "sydney")
  expect_equal(nrow(result_syd), 2)
})

test_that("apply_manual_excludes is a no-op when no matches", {
  restaurants <- tibble::tibble(
    name = c("Unrelated Venue A", "Unrelated Venue B"),
    suburb = c("Foo", "Bar"),
    source = c("eater", "michelin")
  )
  result <- apply_manual_excludes(restaurants, city = "honolulu")
  expect_equal(nrow(result), 2)
})

test_that("apply_manual_excludes is conservative when the row lacks source info", {
  # Av Restaurant exclude is qualified by source = "thrillist". A
  # row without a `source` column can't be verified to match, so we
  # leave it alone rather than risk dropping a different venue
  # that happens to share the name.
  restaurants <- tibble::tibble(
    name = c("Av Restaurant"),
    suburb = c(NA_character_)
  )
  result <- apply_manual_excludes(restaurants, city = "honolulu")
  expect_equal(nrow(result), 1)
})
