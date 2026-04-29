# Tests for infer_missing_price() in R/infer_price.R
#
# Rules apply in priority order; the first match wins. These tests
# pin that ordering and verify the price_inferred flag is set correctly.

bare <- function(price_range = NA_integer_,
                 cuisine = NA_character_,
                 description = NA_character_,
                 michelin_distinction = NA_character_) {
  tibble::tibble(
    name = "X",
    price_range = as.integer(price_range),
    cuisine = cuisine,
    description = description,
    michelin_distinction = michelin_distinction
  )
}


test_that("Michelin Three Stars -> 4 (premium)", {
  out <- infer_missing_price(bare(michelin_distinction = "Three Stars"))
  expect_equal(out$price_range, 4L)
  expect_true(out$price_inferred)
})

test_that("Michelin Two Stars -> 4", {
  out <- infer_missing_price(bare(michelin_distinction = "Two Stars"))
  expect_equal(out$price_range, 4L)
})

test_that("Michelin One Star -> 3", {
  out <- infer_missing_price(bare(michelin_distinction = "One Star"))
  expect_equal(out$price_range, 3L)
})

test_that("Michelin Bib Gourmand -> 2 (good value)", {
  out <- infer_missing_price(bare(michelin_distinction = "Bib Gourmand"))
  expect_equal(out$price_range, 2L)
})

test_that("Michelin Selected without Michelin price stays NA", {
  out <- infer_missing_price(bare(michelin_distinction = "Selected"))
  expect_true(is.na(out$price_range))
  expect_false(out$price_inferred)
})


test_that("description-based fine dining hint -> 4", {
  out <- infer_missing_price(bare(
    description = "A 12-course tasting menu in a chef's table setting."
  ))
  expect_equal(out$price_range, 4L)
})

test_that("description-based prix fixe matches", {
  out <- infer_missing_price(bare(description = "Three-course prix fixe."))
  expect_equal(out$price_range, 4L)
})


test_that("cafe cuisine -> 1 (budget)", {
  out <- infer_missing_price(bare(cuisine = "Cafe"))
  expect_equal(out$price_range, 1L)
})

test_that("bakery cuisine -> 1", {
  out <- infer_missing_price(bare(cuisine = "Bakery, Coffee"))
  expect_equal(out$price_range, 1L)
})

test_that("pizza cuisine -> 2", {
  out <- infer_missing_price(bare(cuisine = "Pizza"))
  expect_equal(out$price_range, 2L)
})

test_that("ramen cuisine -> 2", {
  out <- infer_missing_price(bare(cuisine = "Ramen, Japanese"))
  expect_equal(out$price_range, 2L)
})

test_that("unrecognised cuisine stays NA", {
  out <- infer_missing_price(bare(cuisine = "Modern Australian"))
  expect_true(is.na(out$price_range))
  expect_false(out$price_inferred)
})


test_that("existing non-NA price is preserved", {
  out <- infer_missing_price(bare(
    price_range = 2L,
    michelin_distinction = "Three Stars"  # would otherwise be 4
  ))
  expect_equal(out$price_range, 2L)
  expect_false(out$price_inferred)
})


test_that("Michelin rule wins over cuisine rule (priority order)", {
  out <- infer_missing_price(bare(
    michelin_distinction = "One Star",
    cuisine = "Pizza"  # would be 2 alone; Michelin says 3
  ))
  expect_equal(out$price_range, 3L)
})


test_that("vectorized: mixed inputs apply per-row rules", {
  d <- dplyr::bind_rows(
    bare(michelin_distinction = "One Star"),
    bare(cuisine = "Cafe"),
    bare(price_range = 3L),
    bare()
  )
  out <- infer_missing_price(d)
  expect_equal(out$price_range, c(3L, 1L, 3L, NA_integer_))
  expect_equal(out$price_inferred, c(TRUE, TRUE, FALSE, FALSE))
})


test_that("price_inferred column is added even when no rules fire", {
  out <- infer_missing_price(bare())
  expect_true("price_inferred" %in% names(out))
  expect_false(out$price_inferred)
})


test_that("tolerates missing optional columns gracefully", {
  d <- tibble::tibble(name = "X", price_range = NA_integer_)
  out <- infer_missing_price(d)
  expect_true("price_inferred" %in% names(out))
  expect_true(is.na(out$price_range))
})
