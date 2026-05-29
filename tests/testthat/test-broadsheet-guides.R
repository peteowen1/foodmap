# Tests for the Broadsheet editorial-guides scraper (R/scrape_broadsheet_guides.R)
# The bg_parse_guide / bg_item_to_row tests exercise the JSON-LD ItemList path
# without hitting the network - we hand-craft minimal HTML that mirrors what
# Broadsheet emits.

test_that("bg_coerce_price handles integer, dollar-sign string, and unknown", {
  expect_equal(bg_coerce_price(2L), 2L)
  expect_equal(bg_coerce_price(3),  3L)
  expect_equal(bg_coerce_price("$$"),   2L)
  expect_equal(bg_coerce_price("$$$$"), 4L)
  expect_equal(bg_coerce_price("$$$$$"), 4L)  # clamped to 4
  expect_true(is.na(bg_coerce_price(NULL)))
  expect_true(is.na(bg_coerce_price("expensive")))
  expect_true(is.na(bg_coerce_price(0)))
  expect_true(is.na(bg_coerce_price(5)))
})

test_that("bg_item_to_row pulls name/address/url out of a CafeOrCoffeeShop item", {
  guide <- list(slug = "best-cafes", category = "Cafe", cuisine = "Coffee")
  item <- list(
    `@type` = "CafeOrCoffeeShop",
    name = "Pina",
    url = "https://example.com/pina",
    description = "Heaving brunch spot.",
    telephone = "0432445342",
    priceRange = 2L,
    address = list(
      streetAddress = "Orwell Street",
      addressLocality = "Potts Point",
      postalCode = "2011",
      addressRegion = "sydney"
    )
  )
  row <- bg_item_to_row(item, guide = guide, city = "sydney")
  expect_equal(nrow(row), 1L)
  expect_equal(row$name, "Pina")
  expect_equal(row$suburb, "Potts Point")
  expect_equal(row$category, "Cafe")
  expect_equal(row$cuisine, "Coffee")
  expect_equal(row$price_range, 2L)
  expect_match(row$address, "Orwell Street", fixed = TRUE)
  expect_match(row$address, "Potts Point", fixed = TRUE)
  expect_match(row$address, "2011", fixed = TRUE)
  expect_equal(row$url, "https://example.com/pina")
})

test_that("bg_item_to_row returns NULL when name is missing", {
  guide <- list(slug = "best-cafes", category = "Cafe", cuisine = "Coffee")
  item <- list(`@type` = "CafeOrCoffeeShop", url = "https://example.com")
  expect_null(bg_item_to_row(item, guide = guide, city = "sydney"))
  # And explicitly NA name too.
  item$name <- NA_character_
  expect_null(bg_item_to_row(item, guide = guide, city = "sydney"))
})

test_that("bg_item_to_row uses guide category when JSON-LD type would override it", {
  # Real Broadsheet guides mix BarOrPub + Restaurant in the same ItemList
  # (e.g. "best-cocktails" includes the wine list at a restaurant). We
  # take the guide's category as source-of-truth rather than the schema
  # type, otherwise a "best-cocktails" bar showing up as Restaurant
  # would scramble the cafe/bar count.
  guide <- list(slug = "best-cocktails", category = "Bar", cuisine = "Cocktail Bar")
  item <- list(
    `@type` = "Restaurant",  # ignored
    name = "Wine Hall",
    address = list(addressLocality = "Surry Hills")
  )
  row <- bg_item_to_row(item, guide = guide, city = "sydney")
  expect_equal(row$category, "Bar")
  expect_equal(row$cuisine, "Cocktail Bar")
})

test_that("bg_parse_guide extracts venues from a minimal Broadsheet-style HTML", {
  guide <- list(slug = "best-cafes", category = "Cafe", cuisine = "Coffee")
  # Two-venue ItemList wrapped in the same <script> block Broadsheet uses.
  item_list_json <- jsonlite::toJSON(
    list(
      `@context`        = "https://schema.org",
      `@type`           = "ItemList",
      name              = "Best cafes",
      itemListOrder     = "Unordered",
      itemListElement   = list(
        list(
          `@type` = "ListItem", position = 1L,
          item = list(
            `@type` = "CafeOrCoffeeShop",
            name = "Pina", url = "https://x/pina",
            priceRange = 2L,
            address = list(addressLocality = "Potts Point")
          )
        ),
        list(
          `@type` = "ListItem", position = 2L,
          item = list(
            `@type` = "CafeOrCoffeeShop",
            name = "Edition", url = "https://x/edition",
            address = list(addressLocality = "Haymarket")
          )
        )
      )
    ),
    auto_unbox = TRUE
  )
  html <- sprintf(
    "<html><head><script type='application/ld+json'>%s</script></head>
     <body></body></html>",
    item_list_json
  )
  out <- bg_parse_guide(html, guide = guide, city = "sydney")
  expect_equal(nrow(out), 2L)
  expect_setequal(out$name, c("Pina", "Edition"))
  expect_true(all(out$category == "Cafe"))
})

test_that("bg_parse_guide returns NULL when no ItemList block exists", {
  guide <- list(slug = "best-cafes", category = "Cafe", cuisine = "Coffee")
  # Only an Article block, no ItemList. Real Broadsheet pages do have an
  # Article block but always also an ItemList; this tests our defence
  # against guide-restructure changes upstream.
  html <- "<html><head><script type='application/ld+json'>
            {\"@type\":\"Article\",\"headline\":\"x\"}
          </script></head><body></body></html>"
  expect_null(bg_parse_guide(html, guide = guide, city = "sydney"))
})

test_that("broadsheet_guides_for_city covers sydney + melbourne but errors elsewhere", {
  syd <- broadsheet_guides_for_city("sydney")
  mel <- broadsheet_guides_for_city("melbourne")
  expect_true(length(syd) >= 5)
  expect_true(length(mel) >= 5)
  # Each entry must carry the keys the parser expects.
  for (g in c(syd, mel)) {
    expect_true(all(c("slug", "category", "cuisine") %in% names(g)))
  }
  expect_error(broadsheet_guides_for_city("brisbane"),
               "No Broadsheet guides configured")
})
