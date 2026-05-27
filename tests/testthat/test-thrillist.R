# Tests for Thrillist scraper helpers in R/scrape_thrillist.R

# --- thrillist_parse_guide ---------------------------------------------------

test_that("Restaurant + GeoCoordinates blocks yield rows with coords", {
  html <- paste0(
    '<html>...',
    '"@type":"GeoCoordinates","latitude":"21.275","longitude":"-157.786"},',
    '"image":"x","@type":"Restaurant","name":"Kapa Hale",',
    '"url":"https://thrillist.com/venues/kapa-hale-5070355"',
    ' more html ',
    '"@type":"GeoCoordinates","latitude":"21.30","longitude":"-157.86"},',
    '"image":"y","@type":"Restaurant","name":"Bar Podmore",',
    '"url":"https://thrillist.com/venues/bar-podmore-5070358"',
    '</html>'
  )
  result <- thrillist_parse_guide(html, "https://thrillist.com/eat/honolulu/best-restaurants-honolulu")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$name[1], "Kapa Hale")
  expect_equal(result$latitude[1], 21.275)
  expect_equal(result$longitude[1], -157.786)
  expect_equal(result$name[2], "Bar Podmore")
})

test_that("duplicate Restaurant blocks (carousel/slot) collapse to one row", {
  block <- paste0(
    '"@type":"GeoCoordinates","latitude":"21.275","longitude":"-157.786"},',
    '"image":"x","@type":"Restaurant","name":"Kapa Hale",',
    '"url":"https://thrillist.com/venues/kapa-hale-5070355"'
  )
  html <- paste(rep(block, 3), collapse = " ")
  result <- thrillist_parse_guide(html, "x")
  expect_equal(nrow(result), 1)
  expect_equal(result$name[1], "Kapa Hale")
})

test_that("description is extracted from following H2 + paragraph", {
  html <- paste0(
    '<html>',
    '"@type":"GeoCoordinates","latitude":"21.27","longitude":"-157.78"},',
    '"@type":"Restaurant","name":"Kapa Hale","url":"https://t/v/kh"',
    '... more JSON-LD ...',
    '<h2 class="LocationListItemTitle">Kapa Hale</h2>',
    '<p>The gist: Chef Keaka Lee’s contemporary Hawai‘i tasting menu.</p>',
    '</html>'
  )
  result <- thrillist_parse_guide(html, "x")
  expect_equal(nrow(result), 1)
  expect_true(grepl("Chef Keaka", result$description[1]))
})

test_that("empty html returns NULL not an error", {
  expect_null(thrillist_parse_guide("", "x"))
  expect_null(thrillist_parse_guide("<html>no venues</html>", "x"))
})
