# Tests for Michelin scraper helpers in R/scrape_michelin.R
#
# We exercise the pure helpers (no network): distinction normaliser,
# price-text parser, response validator, and listing-card URL filter.

# --- michelin_normalize_distinction -----------------------------------------

test_that("verbose star strings normalise to short labels", {
  expect_equal(michelin_normalize_distinction("Three Stars: Exceptional cuisine"), "Three Stars")
  expect_equal(michelin_normalize_distinction("Two Stars: Excellent cooking"),     "Two Stars")
  expect_equal(michelin_normalize_distinction("One Star: High quality cooking"),   "One Star")
  expect_equal(michelin_normalize_distinction("Bib Gourmand: Good quality, good value cooking"),
               "Bib Gourmand")
})

test_that("missing star info defaults to Selected (still on city listing)", {
  expect_equal(michelin_normalize_distinction(NULL),       "Selected")
  expect_equal(michelin_normalize_distinction(NA),         "Selected")
  expect_equal(michelin_normalize_distinction(""),         "Selected")
  expect_equal(michelin_normalize_distinction("Selected"), "Selected")
})

test_that("distinction matching is case-insensitive", {
  expect_equal(michelin_normalize_distinction("ONE STAR"),     "One Star")
  expect_equal(michelin_normalize_distinction("bib gourmand"), "Bib Gourmand")
})


# --- michelin_price_to_int --------------------------------------------------

test_that("known Michelin price tiers map to 1-4", {
  expect_equal(michelin_price_to_int("Easy on the wallet"), 1L)
  expect_equal(michelin_price_to_int("A reasonable spend"), 2L)
  expect_equal(michelin_price_to_int("Special occasion"),   3L)
  expect_equal(michelin_price_to_int("Spare no expense"),   4L)
})

test_that("unrecognised price text returns NA", {
  expect_true(is.na(michelin_price_to_int("???")))
  expect_true(is.na(michelin_price_to_int(NULL)))
  expect_true(is.na(michelin_price_to_int(NA)))
})


# --- michelin_response_ok ---------------------------------------------------

test_that("response validator accepts a real Restaurant page", {
  ok <- '<html><script type="application/ld+json">{"@type":"Restaurant"}</script></html>'
  expect_true(michelin_response_ok(ok))
})

test_that("response validator rejects an AWS WAF challenge body", {
  waf <- '<html>AwsWafIntegration challenge ...</html>'
  expect_false(michelin_response_ok(waf))
})

test_that("response validator rejects pages without a Restaurant block", {
  no_ld <- "<html>nothing here</html>"
  expect_false(michelin_response_ok(no_ld))
})


# --- michelin_extract_card_urls ---------------------------------------------

test_that("listing card URLs are filtered to the requested region prefix", {
  html <- paste0(
    'foo <a href="/us/en/california/san-francisco/restaurant/aphotic">x</a> ',
    'bar <a href="/us/en/new-york/restaurant/eleven-madison">y</a> ',
    'baz <a href="/us/en/california/oakland/restaurant/commis">z</a>'
  )
  out <- michelin_extract_card_urls(html, url_prefix = "/us/en/california/")
  expect_length(out, 2)
  expect_true(any(grepl("aphotic", out)))
  expect_true(any(grepl("commis",  out)))
  expect_false(any(grepl("eleven-madison", out)))
})

test_that("listing card URLs return character(0) on empty input", {
  expect_equal(michelin_extract_card_urls("", "/us/en/california/"), character(0))
  expect_equal(michelin_extract_card_urls("<html>no cards</html>", "/us/en/california/"),
               character(0))
})

test_that("listing card URLs are de-duplicated", {
  html <- paste0(
    '<a href="/us/en/california/san-francisco/restaurant/aphotic">x</a> ',
    '<a href="/us/en/california/san-francisco/restaurant/aphotic">x</a>'
  )
  out <- michelin_extract_card_urls(html, url_prefix = "/us/en/california/")
  expect_length(out, 1)
})
