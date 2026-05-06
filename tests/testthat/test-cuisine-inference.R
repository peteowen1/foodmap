# Tests for cuisine-inference helpers added to scrape_eater.R and
# scrape_7x7.R. These cover the keyword maps that turn slug / dish text
# into a cuisine tag for guides that don't expose it directly.

test_that("eater_slug_to_cuisine maps known cuisine slugs", {
  expect_equal(eater_slug_to_cuisine("best-pizza-san-francisco"), "Pizza")
  expect_equal(eater_slug_to_cuisine("best-coffee-shops-san-francisco"), "Coffee")
  expect_equal(eater_slug_to_cuisine("best-ice-cream-san-francisco"), "Ice Cream")
  expect_equal(eater_slug_to_cuisine("best-brunch-san-francisco"), "Brunch")
  expect_equal(eater_slug_to_cuisine("best-steakhouses-san-francisco"), "Steakhouse")
})

test_that("eater_slug_to_cuisine returns NA for flagship lists", {
  expect_true(is.na(eater_slug_to_cuisine("best-restaurants-san-francisco-38")))
  expect_true(is.na(eater_slug_to_cuisine("best-new-restaurants-san-francisco")))
  expect_true(is.na(eater_slug_to_cuisine("")))
  expect_true(is.na(eater_slug_to_cuisine(NA_character_)))
})

test_that("seven_dish_to_cuisine handles canonical SF dishes", {
  cases <- list(
    "Margherita pizza"          = "Pizza",
    "Tonkotsu ramen"            = "Ramen",
    "Cheeseburger with fries"   = "Burgers",
    "Char siu pork bao"         = "Cantonese",
    "Pho dac biet"              = "Vietnamese",
    "Sushi omakase"             = "Japanese",
    "Bibimbap"                  = "Korean",
    "Pad thai with prawns"      = "Thai",
    "Chicken tikka masala"      = "Indian",
    "Carnitas tacos al pastor"  = "Mexican",
    "Hummus and falafel"        = "Middle Eastern",
    "Croissant"                 = "French",
    "Pappardelle bolognese"     = "Pasta",
    "Ribeye steak"              = "Steakhouse",
    "Smoked brisket"            = "Barbecue",
    "Chocolate chip cookie"     = "Bakery/Cafe",
    "Cortado"                   = "Coffee",
    "Gelato"                    = "Ice Cream"
  )
  for (dish in names(cases)) {
    expect_equal(seven_dish_to_cuisine(dish), cases[[dish]],
                 info = paste("dish:", dish))
  }
})

test_that("seven_dish_to_cuisine normalises diacritics", {
  # Vietnamese dishes commonly carry diacritics ("Phở", "bánh mì") -
  # the parser must collapse those to ASCII before matching.
  expect_equal(seven_dish_to_cuisine("Phở Đặc Biệt"), "Vietnamese")
  expect_equal(seven_dish_to_cuisine("Bánh mì"), "Vietnamese")
})

test_that("seven_dish_to_cuisine prefers specific over general", {
  # "char siu bao" is Cantonese - we don't want Vietnamese to win on
  # the word "bao" being a generic Asian bun shape.
  expect_equal(seven_dish_to_cuisine("char siu bao"), "Cantonese")
  # "pizza pasta combo" - Pizza wins because it's listed first.
  expect_equal(seven_dish_to_cuisine("pizza pasta combo"), "Pizza")
})

test_that("seven_dish_to_cuisine returns NA when nothing matches", {
  expect_true(is.na(seven_dish_to_cuisine("Just a generic plate of food")))
  expect_true(is.na(seven_dish_to_cuisine("")))
  expect_true(is.na(seven_dish_to_cuisine(NA_character_)))
})

test_that("prose_to_cuisine fires on identity-anchored adjectives", {
  expect_equal(
    prose_to_cuisine("This Italian restaurant in the Mission..."),
    "Italian"
  )
  expect_equal(
    prose_to_cuisine("A taqueria in the heart of the city."),
    "Mexican"
  )
  expect_equal(
    prose_to_cuisine("This izakaya serves yakitori and sake."),
    "Japanese"
  )
  expect_equal(
    prose_to_cuisine("A trattoria with handmade pasta."),
    "Italian"
  )
})

test_that("prose_to_cuisine ignores ingredient-context adjectives", {
  # The Turtle Tower regression: a Vietnamese place mentioned a
  # "Japanese whiskey drink" and the previous version tagged it as
  # Japanese. The identity-anchored patterns now require a venue noun.
  expect_true(is.na(prose_to_cuisine(
    "A pho shop with a cocktail menu featuring Japanese whiskey."
  )))
  # Generic ingredient mentions don't fire either
  expect_true(is.na(prose_to_cuisine(
    "The chef uses Italian sausage and Greek yogurt."
  )))
})

test_that("prose_to_cuisine excludes 'bistro' and 'tasting menu'", {
  # Bistro is too generic in casual American usage ("bi-level bistro"
  # at Zuni Cafe).
  expect_true(is.na(prose_to_cuisine("This bi-level bistro on the corner.")))
  # Tasting menu is a format used across many cuisines.
  expect_true(is.na(prose_to_cuisine("They serve a 12-course tasting menu.")))
})

test_that("prose_to_cuisine handles diacritics + NA", {
  expect_equal(
    prose_to_cuisine("Café Soleil is a French bistro... bistros aside, this is French cuisine."),
    "French"
  )
  expect_true(is.na(prose_to_cuisine(NA_character_)))
  expect_true(is.na(prose_to_cuisine("")))
})
