# Tests for export_diagnostics() in R/export_diagnostics.R

test_that("export_diagnostics flags rows missing latitude or address", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  path <- file.path(td, "diag.csv")

  rest <- tibble::tibble(
    name      = c("Aria", "Quay", "Tetsuya"),
    suburb    = c("Sydney", "Sydney", "Sydney"),
    address   = c("1 Macquarie St", NA_character_, "529 Kent St"),
    latitude  = c(-33.86, NA_real_, -33.87),
    longitude = c(151.21, NA_real_, 151.20)
  )

  out <- export_diagnostics(rest, path,
                            fields = c("latitude", "address"))
  expect_equal(out, path)
  diag <- utils::read.csv(path, stringsAsFactors = FALSE)
  # Quay has both latitude AND address missing - count = 2; first row.
  expect_equal(nrow(diag), 1)
  expect_equal(diag$name, "Quay")
  expect_equal(diag$issue_count, 2)
  expect_match(diag$issues, "latitude")
  expect_match(diag$issues, "address")
})

test_that("export_diagnostics returns NULL when nothing is flagged", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  path <- file.path(td, "diag.csv")

  rest <- tibble::tibble(
    name      = "Aria",
    address   = "1 Macquarie St",
    latitude  = -33.86,
    longitude = 151.21
  )
  expect_null(suppressMessages(
    export_diagnostics(rest, path, fields = c("latitude", "address"))
  ))
  # And no file should have been written.
  expect_false(file.exists(path))
})

test_that("export_diagnostics sorts worst-first by issue_count", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  path <- file.path(td, "diag.csv")

  rest <- tibble::tibble(
    name        = c("A", "B", "C"),
    latitude    = c(NA_real_, -33.86, NA_real_),
    address     = c(NA_character_, NA_character_, "x"),
    description = c(NA_character_, "ok", "ok")
  )
  # A: missing 3 (latitude, address, description) - count 3
  # B: missing 1 (address)                       - count 1
  # C: missing 1 (latitude)                       - count 1
  suppressMessages(export_diagnostics(
    rest, path,
    fields = c("latitude", "address", "description")
  ))
  diag <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_equal(diag$name[1], "A")  # worst-first ordering
  expect_equal(diag$issue_count[1], 3)
})

test_that("export_diagnostics warns when no requested fields exist", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  path <- file.path(td, "diag.csv")
  rest <- tibble::tibble(name = "Aria")
  expect_warning(
    res <- export_diagnostics(rest, path, fields = c("latitude")),
    "diagnostic fields exist"
  )
  expect_null(res)
})

test_that("is_blank_field treats empty + whitespace strings as blank", {
  # The character path catches the scrapers' common 'returned but empty'
  # cases - some sources emit "" or "   " when a field is unparseable.
  expect_equal(
    is_blank_field(c("ok", "", "  ", NA_character_)),
    c(FALSE, TRUE, TRUE, TRUE)
  )
})

test_that("is_blank_field treats 0 as a real numeric value, not blank", {
  # price_range = 0 means "no $ tier given" and isn't a gap per se -
  # only NA is missing data on numeric columns. is_blank_field reflects
  # that so price=0 doesn't get flagged as a diagnostic issue.
  expect_equal(
    is_blank_field(c(0, 1, NA_real_)),
    c(FALSE, FALSE, TRUE)
  )
})
