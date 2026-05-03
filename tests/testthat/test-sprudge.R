# Tests for Sprudge scraper helpers in R/scrape_sprudge.R
#
# These tests cover the URL filtering and title-cleaning logic that
# don't require live HTTP. The full network-driven scrape is exercised
# manually via analysis/test_sprudge.R.

test_that("sprudge_is_spotlight matches the three known SF series", {
  urls <- c(
    "https://sprudge.com/coffee-design-saint-frank-coffee-in-san-francisco-ca-203792.html",
    "https://sprudge.com/sprudge-maps-spotlight-round-house-cafe-by-equator-coffees-in-san-francisco-ca-182246.html",
    "https://sprudge.com/build-outs-of-summer-coffee-bars-third-location-in-san-francisco-41384.html"
  )
  expect_true(all(sprudge_is_spotlight(urls, "san-francisco")))
})

test_that("sprudge_is_spotlight rejects non-cafe SF coverage", {
  urls <- c(
    "https://sprudge.com/the-san-francisco-coffee-festival-returns-in-november-151984.html",
    "https://sprudge.com/the-queer-coffee-conference-is-coming-to-san-francisco-183749.html",
    "https://sprudge.com/trader-joes-is-being-sued-because-their-coffee-doesnt-have-enough-caffeine-949021.html",
    "https://sprudge.com/coffee-design-camber-coffee-in-bellingham-washington-2-420101.html"
  )
  expect_true(!any(sprudge_is_spotlight(urls, "san-francisco")))
})

test_that("sprudge_clean_name extracts cafe names from real titles", {
  cases <- list(
    list(
      input  = "Coffee Design: Saint Frank Coffee In San Francisco, CA | Sprudge Coffee",
      expect = "Saint Frank Coffee"
    ),
    list(
      input  = "Sprudge Maps Spotlight: Round House Cafe by Equator Coffees In San Francisco, CA | Sprudge Coffee",
      expect = "Round House Cafe by Equator Coffees"
    ),
    list(
      input  = "Build-Outs Of Summer: Pinhole Coffee In San Francisco | Sprudge Coffee",
      expect = "Pinhole Coffee"
    ),
    list(
      input  = "Build-Outs Of Summer: Equator Coffees, San Francisco | Sprudge Coffee",
      expect = "Equator Coffees"
    ),
    list(
      input  = "Build-Outs Of Summer: Coffee Bar's Third Location In San Francisco | Sprudge Coffee",
      expect = "Coffee Bar"
    )
  )
  for (case in cases) {
    expect_equal(sprudge_clean_name(case$input), case$expect)
  }
})

test_that("sprudge_clean_name decodes HTML entities", {
  expect_equal(
    sprudge_clean_name("Coffee Design: Saint Frank&#039;s Coffee In San Francisco, CA | Sprudge Coffee"),
    "Saint Frank's Coffee"
  )
})

test_that("sprudge_parse_article builds a tibble row from minimal HTML", {
  html <- paste0(
    '<html><head>',
    '<meta property="og:title" content="Coffee Design: Saint Frank Coffee In San Francisco, CA | Sprudge Coffee">',
    '<meta property="og:description" content="The new look for SF\'s Saint Frank Coffee.">',
    '</head></html>'
  )
  row <- sprudge_parse_article(html, "https://sprudge.com/test.html")
  expect_equal(row$name, "Saint Frank Coffee")
  expect_equal(row$suburb, "San Francisco")
  expect_equal(row$cuisine, "Coffee")
  expect_equal(row$category, "Cafe")
  expect_true(is.na(row$latitude))
  expect_true(is.na(row$address))
})
