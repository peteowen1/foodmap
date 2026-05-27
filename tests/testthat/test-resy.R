# Tests for Resy scraper helpers in R/scrape_resy.R

# --- resy_strip_rank --------------------------------------------------------

test_that("leading rank prefix is stripped", {
  expect_equal(resy_strip_rank("1. Kidilum"), "Kidilum")
  expect_equal(resy_strip_rank("12. The Dead Rabbit New York City"),
               "The Dead Rabbit New York City")
})

test_that("non-rank-prefixed names pass through", {
  expect_equal(resy_strip_rank("Kidilum"), "Kidilum")
  expect_equal(resy_strip_rank("3rd Wave Cafe"), "3rd Wave Cafe")
})

test_that("NA pass-through", {
  expect_true(is.na(resy_strip_rank(NA_character_)))
})


# --- resy_price_to_int ------------------------------------------------------

test_that("dollar signs convert to integers", {
  expect_equal(resy_price_to_int("$"),    1L)
  expect_equal(resy_price_to_int("$$"),   2L)
  expect_equal(resy_price_to_int("$$$"),  3L)
  expect_equal(resy_price_to_int("$$$$"), 4L)
  # Five or more dollars cap at 4 (matches the rest of the codebase)
  expect_equal(resy_price_to_int("$$$$$"), 4L)
})

test_that("non-dollar strings return NA", {
  expect_true(is.na(resy_price_to_int("")))
  expect_true(is.na(resy_price_to_int(NA_character_)))
  expect_true(is.na(resy_price_to_int("Cheap")))
})


# --- resy_parse -------------------------------------------------------------

test_that("teaser2 articles parse with coords + meta", {
  html <- paste0(
    '<article class="teaser2 is-active" data-lat="40.7430" data-lng="-73.9922">',
    '<a href="https://resy.com/cities/new-york-ny/venues/kidilum?venueId=95134">',
    '<figure></figure></a>',
    '<div class="teaser2-wrap">',
    '<h3 class="teaser2-title -ff:4 -ts:4"><span><a>1. Kidilum</a></span></h3>',
    '<ul class="teaser2-meta -pb:1">',
    '<li>Flatiron District</li>',
    '<li>Indian</li>',
    '<li>$$</li>',
    '</ul></div></article>'
  )
  res <- resy_parse(html, "https://blog.resy.com/the-hit-list/nyc-restaurants/")

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_equal(res$name[1], "Kidilum")
  expect_equal(res$suburb[1], "Flatiron District")
  expect_equal(res$cuisine[1], "Indian")
  expect_equal(res$price_range[1], 2L)
  expect_equal(res$latitude[1], 40.7430)
  expect_equal(res$longitude[1], -73.9922)
  expect_true(grepl("resy.com/cities/new-york", res$url[1]))
})

test_that("multiple articles parse independently with distinct coords", {
  block <- function(lat, lng, rank, name, neigh, cuisine, price) paste0(
    '<article class="teaser2" data-lat="', lat, '" data-lng="', lng, '">',
    '<a href="https://resy.com/cities/new-york-ny/venues/', tolower(name), '">x</a>',
    '<div class="teaser2-wrap">',
    '<h3 class="teaser2-title"><span><a>', rank, '. ', name, '</a></span></h3>',
    '<ul class="teaser2-meta">',
    '<li>', neigh, '</li><li>', cuisine, '</li><li>', price, '</li>',
    '</ul></div></article>'
  )
  html <- paste(
    block(40.74, -73.99, 1, "Foo", "Flatiron", "Indian",   "$$"),
    block(40.72, -73.98, 2, "Bar", "East Village", "Japanese", "$"),
    collapse = "\n"
  )
  res <- resy_parse(html, "x")
  expect_equal(nrow(res), 2)
  expect_equal(res$latitude, c(40.74, 40.72))
  expect_equal(res$cuisine, c("Indian", "Japanese"))
  expect_equal(res$price_range, c(2L, 1L))
})

test_that("empty html returns NULL not error", {
  expect_null(resy_parse("", "x"))
  expect_null(resy_parse("<html>no venues</html>", "x"))
})
