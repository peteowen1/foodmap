# Tests for assert_venue_count() in R/regression_check.R
# Drives the regression check through a tempdir-shadowed expected_counts.csv
# so the production list under inst/extdata/ doesn't have to match the
# test fixture's numbers.

test_that("assert_venue_count returns the input tibble when above floor", {
  skip_if_not_installed("withr")
  # Run from a tempdir with an inst/extdata/expected_counts.csv shadow.
  td <- withr::local_tempdir()
  withr::local_dir(td)
  dir.create(file.path("inst", "extdata"), recursive = TRUE)
  writeLines(
    c("city,expected_min,note",
      "tinytown,5,test floor"),
    file.path("inst", "extdata", "expected_counts.csv")
  )

  rest <- tibble::tibble(
    name = letters[1:10],
    latitude = seq_len(10) + 0.1
  )
  # 10 georeferenced rows, floor 5 - passes.
  out <- suppressMessages(assert_venue_count(rest, "tinytown"))
  expect_equal(nrow(out), 10L)
  # Returned identically so pipelines can keep piping.
  expect_identical(out, rest)
})

test_that("assert_venue_count aborts when below floor", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  withr::local_dir(td)
  dir.create(file.path("inst", "extdata"), recursive = TRUE)
  writeLines(
    c("city,expected_min,note",
      "tinytown,100,floor higher than tibble row count"),
    file.path("inst", "extdata", "expected_counts.csv")
  )

  rest <- tibble::tibble(
    name = letters[1:5],
    latitude = seq_len(5) + 0.1
  )
  expect_error(
    assert_venue_count(rest, "tinytown"),
    "Regression"
  )
})

test_that("assert_venue_count skips with warning when city not registered", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  withr::local_dir(td)
  dir.create(file.path("inst", "extdata"), recursive = TRUE)
  writeLines(
    c("city,expected_min,note",
      "alpha,10,baseline"),
    file.path("inst", "extdata", "expected_counts.csv")
  )

  rest <- tibble::tibble(
    name = letters[1:3],
    latitude = c(1, 2, 3)
  )
  # City not in CSV - should warn and pass through, not abort.
  expect_warning(
    out <- assert_venue_count(rest, "beta"),
    "No expected_min entry"
  )
  expect_identical(out, rest)
})

test_that("assert_venue_count skips silently when expected_counts.csv is missing", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  # Point at a path that definitely doesn't exist. Default fallback to
  # system.file() would otherwise find the installed copy and skip
  # this warning branch entirely.
  bogus <- file.path(td, "definitely-not-here.csv")
  rest <- tibble::tibble(name = "x", latitude = 1)
  expect_warning(
    out <- assert_venue_count(rest, "any",
                              expected_counts_path = bogus),
    "Expected-counts file not found"
  )
  expect_identical(out, rest)
})

test_that("assert_venue_count counts non-NA latitudes only", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  withr::local_dir(td)
  dir.create(file.path("inst", "extdata"), recursive = TRUE)
  writeLines(
    c("city,expected_min,note",
      "tinytown,4,floor exactly four"),
    file.path("inst", "extdata", "expected_counts.csv")
  )

  # 6 rows total, only 4 georeferenced. Should pass (4 >= 4) but barely.
  rest <- tibble::tibble(
    name = letters[1:6],
    latitude = c(1, 2, NA, 4, NA, 6)
  )
  out <- suppressMessages(assert_venue_count(rest, "tinytown"))
  expect_equal(nrow(out), 6L)

  # Drop one more georeferenced row -> 3 georeferenced, floor 4 -> fail.
  rest$latitude[6] <- NA
  expect_error(assert_venue_count(rest, "tinytown"), "Regression")
})
