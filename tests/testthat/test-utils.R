# Tests for internal helper functions in R/utils.R

test_that("xml_escape handles all special characters", {
  expect_equal(xml_escape("A & B"), "A &amp; B")
  expect_equal(xml_escape("A < B"), "A &lt; B")
  expect_equal(xml_escape("A > B"), "A &gt; B")
  expect_equal(xml_escape('A "B"'), "A &quot;B&quot;")
  expect_equal(xml_escape("A 'B'"), "A &apos;B&apos;")
  # Combined

  expect_equal(xml_escape("<b>Tom's & Jerry's</b>"),
               "&lt;b&gt;Tom&apos;s &amp; Jerry&apos;s&lt;/b&gt;")
})

test_that("xml_escape handles edge cases", {
  expect_equal(xml_escape(""), "")
  expect_equal(xml_escape("no special chars"), "no special chars")
})

test_that("format_price converts integers to dollar signs", {
  expect_equal(format_price(1), "$")
  expect_equal(format_price(2), "$$")
  expect_equal(format_price(3), "$$$")
  expect_equal(format_price(4), "$$$$")
})

test_that("format_price handles NA and zero", {
  expect_equal(format_price(NA_integer_), "")
  expect_equal(format_price(0), "")
})

test_that("valid_sources contains the original AU sources", {
  # Smoke test: the original AU sources must remain registered. We
  # don't pin the exact set here because new sources (US guides,
  # etc.) get added regularly — only check the foundational ones
  # other parts of the package depend on.
  sources <- valid_sources()
  must_have <- c("broadsheet", "gourmet_traveller", "timeout",
                 "urban_list", "agfg", "good_food_guide")
  expect_true(all(must_have %in% sources))
  expect_gte(length(sources), length(must_have))
})

test_that("supported_cities_for_source returns correct cities", {
  expect_true("sydney" %in% supported_cities_for_source("broadsheet"))
  expect_true("melbourne" %in% supported_cities_for_source("timeout"))
  expect_true("gold-coast" %in% supported_cities_for_source("agfg"))
  expect_true("canberra" %in% supported_cities_for_source("agfg"))
})

test_that("supported_cities_for_source rejects unknown source", {
  expect_error(supported_cities_for_source("invalid_source"))
})

test_that("validate_city_source accepts valid combinations", {
  expect_equal(validate_city_source("Sydney", "broadsheet"), "sydney")
  expect_equal(validate_city_source("MELBOURNE", "timeout"), "melbourne")
  expect_equal(validate_city_source("gold-coast", "agfg"), "gold-coast")
})

test_that("validate_city_source rejects invalid combinations", {
  expect_error(validate_city_source("darwin", "timeout"))
  expect_error(validate_city_source("brisbane", "gourmet_traveller"))
})

test_that("empty_restaurant_tibble has correct schema", {
  tbl <- empty_restaurant_tibble()
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 0)
  expected_cols <- c("name", "suburb", "address", "cuisine", "category",
                     "description", "price_range", "rating", "rating_scale",
                     "latitude", "longitude", "url")
  expect_equal(names(tbl), expected_cols)
})

test_that("empty_restaurant_tibble has correct column types", {
  tbl <- empty_restaurant_tibble()
  expect_type(tbl$name, "character")
  expect_type(tbl$price_range, "integer")
  expect_type(tbl$rating, "double")
  expect_type(tbl$latitude, "double")
})

test_that("RATE_LIMIT_SECS is a positive number", {
  expect_true(is.numeric(RATE_LIMIT_SECS))
  expect_true(RATE_LIMIT_SECS > 0)
})

test_that("CUISINE_NAMES is a non-empty character vector", {
  expect_type(CUISINE_NAMES, "character")
  expect_true(length(CUISINE_NAMES) > 20)
  # Should contain key cuisines
  expect_true("italian" %in% CUISINE_NAMES)
  expect_true("japanese" %in% CUISINE_NAMES)
  expect_true("modern australian" %in% CUISINE_NAMES)
})

test_that("broadsheet_url builds correct URL", {
  expect_equal(broadsheet_url("sydney"),
               "https://www.broadsheet.com.au/hotlist/sydney")
  expect_equal(broadsheet_url("Melbourne"),
               "https://www.broadsheet.com.au/hotlist/melbourne")
})

test_that("broadsheet_url rejects unsupported cities", {
  expect_error(broadsheet_url("darwin"))
})
