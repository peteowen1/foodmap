# Tests for deduplicate_restaurants() and its helpers in R/deduplicate.R
#
# Dedup is the most algorithmically dense piece of the package: three
# passes (exact, prefix, address-canonical), suburb cluster mapping,
# and richest-row merging. These tests freeze the known-tricky cases
# documented in the dedup function's comments so future tweaks don't
# silently regress.

base_row <- function(name, suburb, source,
                     address = NA_character_,
                     description = NA_character_,
                     price_range = NA_integer_,
                     latitude = NA_real_,
                     longitude = NA_real_) {
  tibble::tibble(
    name = name, suburb = suburb, address = address,
    cuisine = NA_character_, category = "Restaurant",
    description = description,
    price_range = as.integer(price_range),
    rating = NA_real_, rating_scale = NA_character_,
    latitude = as.double(latitude), longitude = as.double(longitude),
    url = NA_character_, source = source
  )
}


# --- canonical_address ------------------------------------------------------

test_that("canonical_address strips unit prefixes", {
  expect_equal(
    canonical_address("1/70 Princes Hwy, Dandenong"),
    canonical_address("70 Princes Hwy, Dandenong")
  )
  expect_equal(
    canonical_address("Shop 5/850 Heidelberg-Kinglake Rd, Hurstbridge"),
    canonical_address("850 Heidelberg-Kinglake Rd, Hurstbridge")
  )
})

test_that("canonical_address strips parenthetical notes", {
  expect_equal(
    canonical_address("70 Princes Hwy (cnr of Souter St)"),
    canonical_address("70 Princes Hwy")
  )
})

test_that("canonical_address normalises Street/St and Road/Rd", {
  expect_equal(
    canonical_address("123 George Street"),
    canonical_address("123 George St")
  )
  expect_equal(
    canonical_address("4 Esplanade Road"),
    canonical_address("4 Esplanade Rd")
  )
})

test_that("canonical_address returns NA for unparseable input", {
  expect_true(is.na(canonical_address(NA_character_)))
  expect_true(is.na(canonical_address("")))
  expect_true(is.na(canonical_address("Just A Restaurant Name")))
})


# --- exact-name dedup -------------------------------------------------------

test_that("exact name + suburb match merges across sources", {
  data <- dplyr::bind_rows(
    base_row("Quay", "The Rocks", "broadsheet"),
    base_row("Quay", "The Rocks", "timeout")
  )
  result <- deduplicate_restaurants(data)
  expect_equal(nrow(result), 1)
  expect_equal(result$n_sources, 2L)
  expect_true(grepl("broadsheet", result$source))
  expect_true(grepl("timeout", result$source))
})

test_that("NA suburb matches any suburb (wildcard)", {
  data <- dplyr::bind_rows(
    base_row("Yan", "Sydney CBD", "broadsheet"),
    base_row("Yan", NA_character_, "gfg_awards")
  )
  result <- deduplicate_restaurants(data)
  expect_equal(nrow(result), 1)
  expect_equal(result$n_sources, 2L)
})

test_that("name normalisation handles & vs and", {
  data <- dplyr::bind_rows(
    base_row("Bessie's & Alma's", "Surry Hills", "broadsheet"),
    base_row("Bessie's and Alma's", "Surry Hills", "timeout")
  )
  result <- deduplicate_restaurants(data)
  expect_equal(nrow(result), 1)
})


# --- suburb cluster handling ------------------------------------------------

test_that("Sydney CBD cluster merges Barangaroo with Sydney CBD", {
  data <- dplyr::bind_rows(
    base_row("Aalia", "Sydney CBD", "broadsheet"),
    base_row("Aalia", "Barangaroo", "gfg_awards")
  )
  result <- deduplicate_restaurants(data)
  expect_equal(nrow(result), 1)
})

test_that("Mosman cluster merges Balmoral with Mosman", {
  data <- dplyr::bind_rows(
    base_row("Bathers' Pavilion", "Mosman", "agfg"),
    base_row("Bathers' Pavilion", "Balmoral", "timeout")
  )
  result <- deduplicate_restaurants(data)
  expect_equal(nrow(result), 1)
})

test_that("Bondi cluster merges Bondi with North Bondi", {
  data <- dplyr::bind_rows(
    base_row("Sean's", "Bondi", "broadsheet"),
    base_row("Sean's", "North Bondi", "gfg_awards"),
    base_row("Sean's", "Bondi Beach", "timeout")
  )
  result <- deduplicate_restaurants(data)
  expect_equal(nrow(result), 1)
  expect_equal(result$n_sources, 3L)
})


# --- prefix-pass dedup ------------------------------------------------------

test_that("prefix-pass folds short name into longer variant", {
  data <- dplyr::bind_rows(
    base_row("Ester", "Chippendale", "gfg_awards"),
    base_row("Ester Restaurant & Bar", "Chippendale", "broadsheet")
  )
  result <- deduplicate_restaurants(data)
  expect_equal(nrow(result), 1)
})

test_that("prefix-pass folds multiple longer variants via shared short name", {
  data <- dplyr::bind_rows(
    base_row("LuMi", "Pyrmont", "gfg_awards"),
    base_row("LuMi Dining", "Pyrmont", "broadsheet"),
    base_row("LuMi Bar & Dining", "Pyrmont", "timeout")
  )
  result <- deduplicate_restaurants(data)
  expect_equal(nrow(result), 1)
  expect_equal(result$n_sources, 3L)
})

test_that("prefix-pass requires a 4-char minimum to avoid greedy merges", {
  # "Bar" is too short to be a prefix connector
  data <- dplyr::bind_rows(
    base_row("Bar", "Surry Hills", "broadsheet"),
    base_row("Bar Copains", "Surry Hills", "gfg_awards")
  )
  result <- deduplicate_restaurants(data)
  expect_equal(nrow(result), 2)
})


# --- address-pass dedup -----------------------------------------------------

test_that("address-pass merges venues with same canonical address", {
  data <- dplyr::bind_rows(
    base_row("O.My", "Beaconsfield", "broadsheet",
             address = "1/70 Princes Hwy, Beaconsfield"),
    base_row("O.My", "Beaconsfield", "agfg",
             address = "70 Princes Hwy, Beaconsfield")
  )
  result <- deduplicate_restaurants(data)
  expect_equal(nrow(result), 1)
})

test_that("address-pass merges across suburb disagreement", {
  # Askal: Broadsheet says Melbourne, AGFG says East Melbourne
  data <- dplyr::bind_rows(
    base_row("Askal", "Melbourne", "broadsheet",
             address = "167 Exhibition St, Melbourne"),
    base_row("Different Name Same Address", "East Melbourne", "agfg",
             address = "167 Exhibition St, East Melbourne")
  )
  result <- deduplicate_restaurants(data)
  expect_equal(nrow(result), 1)
})


# --- merge semantics --------------------------------------------------------

test_that("merge picks longest description across sources", {
  data <- dplyr::bind_rows(
    base_row("Quay", "The Rocks", "broadsheet",
             description = "Short."),
    base_row("Quay", "The Rocks", "timeout",
             description = "A much longer and more detailed description.")
  )
  result <- deduplicate_restaurants(data)
  expect_equal(result$description,
               "A much longer and more detailed description.")
})

test_that("merge prefers non-NA coordinates", {
  data <- dplyr::bind_rows(
    base_row("Quay", "The Rocks", "broadsheet",
             latitude = NA_real_, longitude = NA_real_),
    base_row("Quay", "The Rocks", "timeout",
             latitude = -33.8568, longitude = 151.21)
  )
  result <- deduplicate_restaurants(data)
  expect_equal(result$latitude, -33.8568)
  expect_equal(result$longitude, 151.21)
})

test_that("merge takes integer median of price_range across sources", {
  data <- dplyr::bind_rows(
    base_row("Quay", "The Rocks", "broadsheet", price_range = 4L),
    base_row("Quay", "The Rocks", "timeout",    price_range = 3L),
    base_row("Quay", "The Rocks", "agfg",       price_range = 4L)
  )
  result <- deduplicate_restaurants(data)
  expect_equal(result$price_range, 4L)
})

test_that("merge caps price at 4 even if a source reports 5", {
  data <- dplyr::bind_rows(
    base_row("Quay", "The Rocks", "broadsheet", price_range = 4L),
    base_row("Quay", "The Rocks", "timeout",    price_range = 5L)
  )
  result <- deduplicate_restaurants(data)
  expect_lte(result$price_range, 4L)
})


# --- pass-through behaviour -------------------------------------------------

test_that("deduplicate_restaurants warns and returns input when source missing", {
  data <- base_row("Quay", "The Rocks", "broadsheet")
  data$source <- NULL
  expect_warning(result <- deduplicate_restaurants(data), "source")
  expect_equal(nrow(result), 1)
})

test_that("single-row inputs get n_sources = 1", {
  data <- base_row("Quay", "The Rocks", "broadsheet")
  result <- deduplicate_restaurants(data)
  expect_equal(result$n_sources, 1L)
})
