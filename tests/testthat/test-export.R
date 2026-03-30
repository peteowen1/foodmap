# Tests for export functions

test_that("export_csv writes a valid CSV file", {
  tbl <- tibble::tibble(
    name = c("Restaurant A", "Restaurant B"),
    suburb = c("Sydney", "Melbourne"),
    address = c("1 George St", "2 Collins St"),
    cuisine = c("Italian", "French"),
    category = c("Restaurant", "Restaurant"),
    description = c("Great pasta", "Fine dining"),
    price_range = c(2L, 4L),
    rating = c(NA_real_, 4.5),
    rating_scale = c(NA_character_, "timeout_stars_5"),
    latitude = c(-33.8688, -37.8136),
    longitude = c(151.2093, 144.9631),
    url = c("https://example.com/a", "https://example.com/b")
  )

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  result <- export_csv(tbl, path)
  expect_equal(result, path)
  expect_true(file.exists(path))

  # Read back and verify
  read_back <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(nrow(read_back), 2)
  expect_equal(read_back$name, c("Restaurant A", "Restaurant B"))
})

test_that("export_csv errors on invalid path", {
  tbl <- tibble::tibble(name = "test")
  expect_error(export_csv(tbl, "/nonexistent/dir/file.csv"), "Failed to write CSV")
})

test_that("export_kml writes a valid KML file", {
  tbl <- tibble::tibble(
    name = c("Restaurant A"),
    suburb = c("Sydney"),
    address = c("1 George St, Sydney"),
    cuisine = c("Italian"),
    category = c("Restaurant"),
    description = c("Great pasta"),
    price_range = c(2L),
    rating = c(NA_real_),
    rating_scale = c(NA_character_),
    latitude = c(-33.8688),
    longitude = c(151.2093),
    url = c("https://example.com/a")
  )

  path <- tempfile(fileext = ".kml")
  on.exit(unlink(path))

  result <- export_kml(tbl, path)
  expect_equal(result, path)
  expect_true(file.exists(path))

  # Parse and verify KML structure
  doc <- xml2::read_xml(path)
  ns <- xml2::xml_ns(doc)
  placemarks <- xml2::xml_find_all(doc, ".//d1:Placemark", ns)
  expect_length(placemarks, 1)

  name <- xml2::xml_text(xml2::xml_find_first(placemarks[[1]], ".//d1:name", ns))
  expect_equal(name, "Restaurant A")
})

test_that("export_kml excludes rows without coordinates", {
  tbl <- tibble::tibble(
    name = c("Has Coords", "No Coords"),
    suburb = c("Sydney", "Melbourne"),
    address = c("1 George St", NA_character_),
    cuisine = c("Italian", "French"),
    category = c("Restaurant", "Restaurant"),
    description = c("A", "B"),
    price_range = c(2L, 3L),
    rating = c(NA_real_, NA_real_),
    rating_scale = c(NA_character_, NA_character_),
    latitude = c(-33.8688, NA_real_),
    longitude = c(151.2093, NA_real_),
    url = c(NA_character_, NA_character_)
  )

  path <- tempfile(fileext = ".kml")
  on.exit(unlink(path))

  expect_warning(export_kml(tbl, path), "Excluding 1 venue")

  doc <- xml2::read_xml(path)
  ns <- xml2::xml_ns(doc)
  placemarks <- xml2::xml_find_all(doc, ".//d1:Placemark", ns)
  expect_length(placemarks, 1)
})

test_that("export_kml aborts when no venues have coordinates", {
  tbl <- tibble::tibble(
    name = "No Coords",
    latitude = NA_real_,
    longitude = NA_real_
  )
  # Suppress the "Excluding" warning to test the subsequent abort
  expect_error(
    suppressWarnings(export_kml(tbl, tempfile())),
    "No venues with coordinates"
  )
})

test_that("export_kml handles special characters in names", {
  tbl <- tibble::tibble(
    name = c("Tom's Bar & Grill <Special>"),
    suburb = c("Sydney"),
    address = c(NA_character_),
    cuisine = c(NA_character_),
    category = c(NA_character_),
    description = c(NA_character_),
    price_range = c(NA_integer_),
    rating = c(NA_real_),
    rating_scale = c(NA_character_),
    latitude = c(-33.8688),
    longitude = c(151.2093),
    url = c(NA_character_)
  )

  path <- tempfile(fileext = ".kml")
  on.exit(unlink(path))

  # Should not error — xml2 auto-escapes text nodes
  export_kml(tbl, path)
  doc <- xml2::read_xml(path)
  ns <- xml2::xml_ns(doc)
  name <- xml2::xml_text(xml2::xml_find_first(doc, ".//d1:Placemark/d1:name", ns))
  expect_equal(name, "Tom's Bar & Grill <Special>")
})
