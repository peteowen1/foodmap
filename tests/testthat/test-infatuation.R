# Tests for Infatuation scraper helpers in R/scrape_infatuation.R

# Build a minimal HTML page that mimics an Infatuation guide: a
# JSON-LD ItemList with two Restaurant entries.
build_infatuation_html <- function() {
  json <- '{
    "@type": "ItemList",
    "itemListElement": [
      {
        "@type": "ListItem",
        "item": {
          "@type": "Restaurant",
          "name": "Tartine Bakery",
          "url": "https://www.theinfatuation.com/sf/restaurants/tartine",
          "description": "Iconic SF bakery.",
          "priceRange": "$$",
          "servesCuisine": ["Bakery", "Cafe"],
          "address": {
            "streetAddress": "600 Guerrero St",
            "addressLocality": "San Francisco",
            "addressRegion": "CA",
            "postalCode": "94110"
          },
          "geo": {"latitude": 37.7614, "longitude": -122.4242}
        }
      },
      {
        "@type": "ListItem",
        "item": {
          "@type": "Restaurant",
          "name": "Emmy&apos;s Spaghetti Shack",
          "description": "Late-night pasta &amp; cocktails.",
          "priceRange": "$$",
          "servesCuisine": "Italian",
          "address": {
            "streetAddress": "18 Virginia Ave",
            "addressLocality": "San Francisco"
          },
          "geo": {"latitude": 37.7421, "longitude": -122.4187}
        }
      }
    ]
  }'
  paste0(
    "<html><head>",
    '<script type="application/ld+json">', json, '</script>',
    "</head><body></body></html>"
  )
}


test_that("infatuation_parse_guide extracts both venues from a JSON-LD list", {
  out <- infatuation_parse_guide(build_infatuation_html())
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2)
  expect_equal(out$name[1], "Tartine Bakery")
  expect_equal(out$cuisine[1], "Bakery, Cafe")  # list joined with ", "
  expect_equal(out$cuisine[2], "Italian")
  expect_equal(out$price_range[1], 2L)          # "$$" -> 2
  expect_equal(out$latitude[1], 37.7614)
})

test_that("HTML entities in names and descriptions are decoded", {
  out <- infatuation_parse_guide(build_infatuation_html())
  expect_equal(out$name[2], "Emmy's Spaghetti Shack")
  expect_equal(out$description[2], "Late-night pasta & cocktails.")
})

test_that("address builder strips trailing NA segments", {
  out <- infatuation_parse_guide(build_infatuation_html())
  expect_equal(out$address[1], "600 Guerrero St, San Francisco, CA, 94110")
  expect_equal(out$address[2], "18 Virginia Ave, San Francisco")
})


test_that("non-Restaurant ItemList entries are skipped", {
  json <- '{
    "@type": "ItemList",
    "itemListElement": [
      {"@type": "ListItem", "item": {"@type": "Article", "name": "skip me"}},
      {"@type": "ListItem", "item": {"@type": "Restaurant", "name": "Keep Me"}}
    ]
  }'
  html <- paste0(
    '<html><script type="application/ld+json">', json, '</script></html>'
  )
  out <- infatuation_parse_guide(html)
  expect_equal(nrow(out), 1)
  expect_equal(out$name, "Keep Me")
})


test_that("no JSON-LD at all returns NULL", {
  out <- infatuation_parse_guide("<html>nothing useful</html>")
  expect_null(out)
})
