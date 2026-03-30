# Schema validation tests — verify all scrapers produce tibbles with the
# correct column names and types. These tests use the venues_to_tibble()
# and parser functions directly with mock data, avoiding network calls.

# Standard columns every scraper must produce
STANDARD_COLS <- c("name", "suburb", "address", "cuisine", "category",
                   "description", "price_range", "rating", "rating_scale",
                   "latitude", "longitude", "url")

STANDARD_TYPES <- list(
  name         = "character",
  suburb       = "character",
  address      = "character",
  cuisine      = "character",
  category     = "character",
  description  = "character",
  price_range  = "integer",
  rating       = "double",
  rating_scale = "character",
  latitude     = "double",
  longitude    = "double",
  url          = "character"
)

# Helper to check a tibble has the standard schema
expect_standard_schema <- function(tbl, label = "tibble") {
  expect_s3_class(tbl, "tbl_df")
  for (col in STANDARD_COLS) {
    expect_true(col %in% names(tbl),
                info = paste(label, "missing column:", col))
  }
  for (col in names(STANDARD_TYPES)) {
    if (col %in% names(tbl) && nrow(tbl) > 0) {
      expect_type(tbl[[col]], STANDARD_TYPES[[col]])
    }
  }
}

test_that("empty_restaurant_tibble matches standard schema", {
  expect_standard_schema(empty_restaurant_tibble(), "empty_restaurant_tibble")
})

test_that("Broadsheet venues_to_tibble produces standard schema", {
  # Minimal mock venue matching Broadsheet API structure
  mock_venues <- list(
    list(
      profile_id = 1L,
      title = "Test Restaurant",
      suburb = "Surry Hills",
      category = "Restaurant",
      hotlist_blurb = "A great place",
      pricerange = 2,
      cuisine = list("Italian", "Modern Australian"),
      primary_address = list(list(
        number = "42", street = "Oxford St",
        latitude = "-33.88", longitude = "151.21"
      )),
      url = "/sydney/food-and-drink/test-restaurant"
    )
  )

  result <- venues_to_tibble(mock_venues, "sydney")
  expect_standard_schema(result, "Broadsheet venues_to_tibble")
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "Test Restaurant")
  expect_equal(result$cuisine, "Italian, Modern Australian")
})

test_that("Broadsheet venues_to_tibble handles missing fields gracefully", {
  mock_venues <- list(
    list(
      profile_id = 2L,
      title = "Minimal Venue",
      primary_address = list(list())
    )
  )

  result <- venues_to_tibble(mock_venues, "sydney")
  expect_standard_schema(result, "Broadsheet minimal")
  expect_equal(result$name, "Minimal Venue")
  expect_true(is.na(result$cuisine))
  expect_true(is.na(result$latitude))
})
