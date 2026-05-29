# Tests for Hale ʻAina scraper helpers in R/scrape_hale_aina.R

# --- hale_aina_parse_paragraph ----------------------------------------------

test_that("standard gold/silver/bronze/finalist paragraphs parse", {
  expect_equal(
    hale_aina_parse_paragraph('GOLD – <strong><a href="x">Café Kaila</a></strong>')$name,
    "Café Kaila"
  )
  expect_equal(
    hale_aina_parse_paragraph('SILVER – <strong>Nami Kaze</strong>')$rank,
    "SILVER"
  )
  expect_equal(
    hale_aina_parse_paragraph('FINALIST – <strong>Mariposa</strong>')$rank_order,
    4L
  )
})

test_that("non-ranked paragraphs return NULL", {
  expect_null(hale_aina_parse_paragraph("Some intro prose"))
  expect_null(hale_aina_parse_paragraph("<p>random text</p>"))
})

test_that("link wrapped outside strong is unwrapped", {
  expect_equal(
    hale_aina_parse_paragraph(
      'GOLD – <a href="x"><strong>Miro Kaimukī</strong></a>'
    )$name,
    "Miro Kaimukī"
  )
})


# --- hale_aina_is_skip_category ---------------------------------------------

test_that("outer-island categories are skipped", {
  expect_true(hale_aina_is_skip_category("Best Maui Restaurant"))
  expect_true(hale_aina_is_skip_category("Best Hawai'i Island Restaurant"))
  expect_true(hale_aina_is_skip_category("Best Kaua'i Restaurant"))
})

test_that("person-not-place categories are skipped", {
  expect_true(hale_aina_is_skip_category("Restaurateur of the Year"))
})

test_that("nav/sidebar categories are skipped", {
  expect_true(hale_aina_is_skip_category("Related"))
  expect_true(hale_aina_is_skip_category("Most Popular"))
  expect_true(hale_aina_is_skip_category("Promotional Content"))
})

test_that("regular categories are kept", {
  expect_false(hale_aina_is_skip_category("Best Izakaya"))
  expect_false(hale_aina_is_skip_category("Best O'ahu Restaurant"))
  expect_false(hale_aina_is_skip_category("Best New Restaurant"))
})


# --- hale_aina_extract_year -------------------------------------------------

test_that("year is pulled from H1", {
  expect_equal(
    hale_aina_extract_year('<h1 class="entry-title">2025 Hale ʻAina Award Winners</h1>'),
    2025L
  )
})

test_that("missing year falls back to current year", {
  expect_equal(
    hale_aina_extract_year("<h1>Something else</h1>"),
    as.integer(format(Sys.Date(), "%Y"))
  )
})

test_that("hale_aina_classify_award routes bars + cafes to the right category", {
  # Bar paths (most specific first; cocktail/brewery/tiki/wine before bare bar)
  expect_equal(hale_aina_classify_award("Best Cocktail Bar"),
               c("Bar", "Cocktail Bar"))
  expect_equal(hale_aina_classify_award("Best Brewery"),
               c("Bar", "Brewery"))
  expect_equal(hale_aina_classify_award("Best Tiki Bar"),
               c("Bar", "Tiki Bar"))
  expect_equal(hale_aina_classify_award("Best Wine Bar"),
               c("Bar", "Wine Bar"))
  expect_equal(hale_aina_classify_award("Best Bar"),
               c("Bar", "Bar"))
  # Cafe paths
  expect_equal(hale_aina_classify_award("Best Coffee Shop"),
               c("Cafe", "Coffee"))
  expect_equal(hale_aina_classify_award("Best Cafe"),
               c("Cafe", "Cafe"))
  expect_equal(hale_aina_classify_award("Best Bakery"),
               c("Cafe", "Bakery"))
  expect_equal(hale_aina_classify_award("Best Brunch"),
               c("Cafe", "Breakfast"))
  # Restaurant catchall (the existing behaviour for all the other ~30
  # award categories Hale Aina runs)
  expect_equal(hale_aina_classify_award("Best Sushi"),
               c("Restaurant", ""))
  expect_equal(hale_aina_classify_award("Best Steak"),
               c("Restaurant", ""))
  # Edge cases - NA/empty award text should fall through cleanly
  # rather than throwing, since hale_aina_to_tibble vapplys this and
  # any throw would lose the whole tibble.
  expect_equal(hale_aina_classify_award(NA_character_),
               c("Restaurant", ""))
  expect_equal(hale_aina_classify_award(""),
               c("Restaurant", ""))
})
