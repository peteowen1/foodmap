# Tests for harmonize_sources() and its label helpers in R/harmonize.R

basic_data <- function(...) {
  rows <- list(...)
  defaults <- tibble::tibble(
    name = "X", suburb = "Y", address = NA_character_,
    cuisine = NA_character_, category = "Restaurant",
    description = NA_character_,
    price_range = NA_integer_,
    rating = NA_real_, rating_scale = NA_character_,
    latitude = NA_real_, longitude = NA_real_,
    url = NA_character_, source = "broadsheet"
  )
  dplyr::bind_rows(lapply(rows, function(r) {
    out <- defaults
    for (k in names(r)) out[[k]] <- r[[k]]
    out
  }))
}


test_that("harmonize_sources errors when source column missing", {
  d <- tibble::tibble(name = "X")
  expect_error(harmonize_sources(d), "source")
})


test_that("harmonize_sources synthesises n_sources from source string", {
  d <- basic_data(
    list(name = "A", source = "broadsheet"),
    list(name = "B", source = "broadsheet, timeout"),
    list(name = "C", source = "broadsheet, timeout, agfg")
  )
  out <- harmonize_sources(d)
  expect_true("n_sources" %in% names(out))
  expect_equal(out$n_sources, c(1L, 2L, 3L))
})

test_that("harmonize_sources preserves existing n_sources", {
  d <- basic_data(list(source = "broadsheet, timeout"))
  d$n_sources <- 99L  # weird but caller-supplied
  out <- harmonize_sources(d)
  expect_equal(out$n_sources, 99L)
})


test_that("price_label adds (est.) suffix when price_inferred is TRUE", {
  d <- basic_data(
    list(name = "Real",     price_range = 3L),
    list(name = "Inferred", price_range = 2L)
  )
  d$price_inferred <- c(FALSE, TRUE)
  out <- harmonize_sources(d)
  expect_equal(out$price_label[1], "$$$")
  expect_equal(out$price_label[2], "$$ (est.)")
})

test_that("price_label is NA when price_range is NA", {
  d <- basic_data(list(price_range = NA_integer_))
  out <- harmonize_sources(d)
  expect_true(is.na(out$price_label))
})

test_that("cost_bracket prefers explicit cost_range over derived", {
  d <- basic_data(list(price_range = 3L))
  d$cost_range <- "Special occasion"
  out <- harmonize_sources(d)
  expect_equal(out$cost_bracket, "Special occasion")
})

test_that("cost_bracket falls back to derived from price_range", {
  d <- basic_data(list(price_range = 3L))
  out <- harmonize_sources(d)
  expect_equal(out$cost_bracket, "$60-$100")
})


test_that("rating_label uses Michelin distinction when present", {
  d <- basic_data(list(rating = 4.5, rating_scale = "timeout_stars_5"))
  d$michelin_distinction <- "One Star"
  out <- harmonize_sources(d)
  expect_equal(out$rating_label, "Michelin: One Star")
})

test_that("rating_label uses GFG hats when present", {
  d <- basic_data(list(rating = 2, rating_scale = "gfg_hats_3"))
  d$hats <- 2
  out <- harmonize_sources(d)
  expect_equal(out$rating_label, "2 hats")
})

test_that("rating_label formats Time Out stars", {
  d <- basic_data(list(rating = 4, rating_scale = "timeout_stars_5"))
  out <- harmonize_sources(d)
  expect_equal(out$rating_label, "4/5 stars")
})

test_that("rating_label formats AGFG /19 score", {
  d <- basic_data(list(rating = 17, rating_scale = "agfg_hats_19"))
  out <- harmonize_sources(d)
  expect_equal(out$rating_label, "17/19")
})
