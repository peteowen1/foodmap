# Tests for W50B scraper helpers in R/scrape_worlds50best.R

# --- w50b_extract_detail_urls ----------------------------------------------

test_that("listing page yields detail URLs prefixed with base", {
  html <- paste0(
    '<div class="list-item"><a href="/the-list/maido.html">x</a></div>',
    '<div class="list-item"><a href="/the-list/asador-etxebarri.html">x</a></div>'
  )
  urls <- w50b_extract_detail_urls(html)
  expect_length(urls, 2)
  expect_true(all(startsWith(urls, "https://www.theworlds50best.com/the-list/")))
})

test_that("empty / no-match returns empty character", {
  expect_length(w50b_extract_detail_urls(""), 0)
  expect_length(w50b_extract_detail_urls("<html>no links</html>"), 0)
})

test_that("duplicates in listing are removed", {
  html <- paste0(
    '<a href="/the-list/maido.html">x</a>',
    '<a href="/the-list/maido.html">y</a>'
  )
  urls <- w50b_extract_detail_urls(html)
  expect_length(urls, 1)
})


# --- w50b_extract_jsonld_restaurant ----------------------------------------

test_that("Restaurant JSON-LD is extracted from detail page", {
  ld <- paste0(
    '<script type="application/ld+json">',
    '{"@context":"https://schema.org","@type":"Restaurant",',
    '"name":"Maido","address":{"streetAddress":"San Martin 399","addressLocality":"Lima"},',
    '"description":"Nikkei tasting menu"}',
    '</script>'
  )
  parsed <- w50b_extract_jsonld_restaurant(ld)
  expect_equal(parsed$name, "Maido")
  expect_equal(parsed$address$addressLocality, "Lima")
})

test_that("non-Restaurant JSON-LD blocks are skipped", {
  ld <- paste0(
    '<script type="application/ld+json">',
    '{"@type":"BreadcrumbList","itemListElement":[]}',
    '</script>'
  )
  expect_null(w50b_extract_jsonld_restaurant(ld))
})


# --- w50b_extract_award_year -----------------------------------------------

test_that("years are extracted from award citations", {
  awards <- list(
    "The World's 50 Best Restaurants 2025, No. 1",
    "The Best Restaurant in South America 2024"
  )
  expect_equal(w50b_extract_award_year(awards), 2025L)
})

test_that("missing or unparseable awards return NA", {
  expect_true(is.na(w50b_extract_award_year(list())))
  expect_true(is.na(w50b_extract_award_year(list("No year here"))))
})


# --- w50b_city_localities --------------------------------------------------

test_that("city slugs map to localities", {
  expect_true("New York" %in% w50b_city_localities("new-york"))
  expect_true("Los Angeles" %in% w50b_city_localities("los-angeles"))
  expect_true("Santa Monica" %in% w50b_city_localities("los-angeles"))
  expect_true("London" %in% w50b_city_localities("london"))
  expect_true("San Francisco" %in% w50b_city_localities("san-francisco"))
})

test_that("unknown city aborts", {
  expect_error(w50b_city_localities("atlantis"))
})
