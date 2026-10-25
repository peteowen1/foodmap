# scrape_all_sources() must name every source that failed, came back
# empty, or quietly used a committed fallback (e.g. the Michelin WAF
# snapshot), rather than leaving that only in scattered per-source warnings.

venue_tbl <- function(n) {
  tibble::tibble(name = sprintf("Venue %d", seq_len(n)),
                 suburb = rep("CBD", n))
}

test_that("degraded_source_lines lists only non-ok sources", {
  status <- list(
    eater    = list(status = "ok", detail = "10 rows"),
    michelin = list(status = "fallback",
                    detail = "committed Michelin snapshot, 378 rows"),
    sprudge  = list(status = "empty", detail = "0 rows"),
    worlds50best = list(status = "failed", detail = "No venue links\nextracted")
  )
  lines <- degraded_source_lines(status)
  expect_equal(lines, c(
    "michelin: fallback (committed Michelin snapshot, 378 rows)",
    "sprudge: empty (0 rows)",
    "worlds50best: failed (No venue links extracted)"
  ))
  expect_equal(degraded_source_lines(status["eater"]), character())
})

test_that("scrape_all_sources reports failed, empty and fallback sources", {
  report <- withr::local_tempfile(fileext = ".md")
  withr::local_envvar(FOODMAP_SOURCE_REPORT = report)
  local_mocked_bindings(
    valid_sources = function() c("eater", "michelin", "sprudge", "worlds50best"),
    supported_cities_for_source = function(source) "london",
    scrape_restaurants = function(city, source, ...) {
      switch(source,
        eater = venue_tbl(3),
        michelin = mark_source_fallback(venue_tbl(2), "committed Michelin snapshot"),
        sprudge = venue_tbl(0),
        worlds50best = cli::cli_abort("No venue links extracted from W50B listing pages.")
      )
    }
  )

  out <- suppressWarnings(suppressMessages(scrape_all_sources("london")))
  expect_equal(nrow(out), 5)

  written <- readLines(report)
  expect_equal(written, c(
    "- **london** michelin: fallback (committed Michelin snapshot, 2 rows)",
    "- **london** sprudge: empty (0 rows)",
    "- **london** worlds50best: failed (No venue links extracted from W50B listing pages.)"
  ))
})

test_that("summary is written even when every source fails", {
  report <- withr::local_tempfile(fileext = ".md")
  withr::local_envvar(FOODMAP_SOURCE_REPORT = report)
  local_mocked_bindings(
    valid_sources = function() "agfg",
    supported_cities_for_source = function(source) "brisbane",
    scrape_restaurants = function(city, source, ...) cli::cli_abort("HTTP 500")
  )
  expect_error(
    suppressWarnings(suppressMessages(scrape_all_sources("brisbane"))),
    "All sources failed"
  )
  expect_equal(readLines(report), "- **brisbane** agfg: failed (HTTP 500)")
})

test_that("no report file is written when nothing is degraded", {
  report <- file.path(withr::local_tempdir(), "report.md")
  withr::local_envvar(FOODMAP_SOURCE_REPORT = report)
  local_mocked_bindings(
    valid_sources = function() "eater",
    supported_cities_for_source = function(source) "london",
    scrape_restaurants = function(city, source, ...) venue_tbl(4)
  )
  suppressMessages(scrape_all_sources("london"))
  expect_false(file.exists(report))
})
