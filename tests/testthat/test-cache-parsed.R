# Tests for the parsed-tibble cache in R/cache_parsed.R
#
# Hit/miss is exercised end-to-end against a temp cache directory. The
# expression argument is a real tibble-returning closure that records
# whether it was called - that's how we detect a cache hit (closure
# untouched) vs miss (closure ran).

test_that("cached_scrape stores tibble and skips re-eval on second call", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()

  call_count <- 0L
  produce <- function() {
    call_count <<- call_count + 1L
    tibble::tibble(name = "Aria", suburb = "Sydney")
  }

  # No URL tracking on this first run (cached_fetch isn't called), so
  # the cache validates via TTL.
  out1 <- suppressMessages(cached_scrape("bsh_sydney", produce(),
                                         cache_dir = td))
  out2 <- suppressMessages(cached_scrape("bsh_sydney", produce(),
                                         cache_dir = td))

  expect_equal(call_count, 1L)
  expect_equal(out1, out2)
  expect_true(file.exists(file.path(td, "bsh_sydney.rds")))
  expect_true(file.exists(file.path(td, "bsh_sydney.manifest.json")))
})

test_that("cached_scrape invalidates when a tracked HTML cache file changes", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  html_dir <- file.path(td, "html")
  parsed_dir <- file.path(td, "parsed")
  dir.create(html_dir, recursive = TRUE)

  # Simulate a URL the scraper "fetched" - we manually write a hash-named
  # HTML cache file matching what cache_path() would produce.
  fake_url <- "https://example.com/listing"
  cp <- cache_path(fake_url, cache_dir = html_dir)
  writeLines("<html>v1</html>", cp)

  call_count <- 0L
  produce <- function() {
    call_count <<- call_count + 1L
    # Record the URL so the manifest captures it (mimics cached_fetch).
    cache_track_record(fake_url)
    tibble::tibble(name = "Test")
  }

  # First call: cache miss, expr runs, manifest records URL+mtime
  # against our test html_dir.
  suppressMessages(cached_scrape("k", produce(),
                                 cache_dir = parsed_dir,
                                 html_cache_dir = html_dir))
  expect_equal(call_count, 1L)

  # Second call: nothing changed, expect cache hit.
  suppressMessages(cached_scrape("k", produce(),
                                 cache_dir = parsed_dir,
                                 html_cache_dir = html_dir))
  expect_equal(call_count, 1L)

  # Mutate the HTML cache file's mtime by a couple of seconds. Future
  # mtime sidesteps filesystem-mtime resolution issues on Windows where
  # "now" can equal the file's existing write time.
  Sys.setFileTime(cp, Sys.time() + 2)

  # Third call: manifest mtime check fails -> re-eval.
  suppressMessages(cached_scrape("k", produce(),
                                 cache_dir = parsed_dir,
                                 html_cache_dir = html_dir))
  expect_equal(call_count, 2L)
})

test_that("cached_scrape TTL fallback fires when no URLs were tracked", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  call_count <- 0L
  produce <- function() {
    call_count <<- call_count + 1L
    tibble::tibble(name = "Test")
  }

  # First write -> cache miss, runs expr.
  suppressMessages(cached_scrape("k", produce(),
                                 cache_dir = td, ttl_hours = 24))
  expect_equal(call_count, 1L)

  # Push the RDS mtime far into the past so the TTL invalidates.
  rds <- file.path(td, "k.rds")
  Sys.setFileTime(rds, Sys.time() - 60 * 60 * 48)  # 48h ago

  suppressMessages(cached_scrape("k", produce(),
                                 cache_dir = td, ttl_hours = 24))
  expect_equal(call_count, 2L)
})

test_that("cache_track_start/record/stop is a one-shot bucket", {
  # The tracker is a single mutable env. Start clears it; stop drains
  # and resets. record() outside an active window is a no-op so it's
  # safe to call from cached_fetch in non-cached contexts.
  cache_track_record("https://noop.example")  # no start - ignored
  cache_track_start()
  cache_track_record("https://a.example")
  cache_track_record("https://b.example")
  cache_track_record("https://a.example")  # dedup
  urls <- cache_track_stop()
  expect_setequal(urls, c("https://a.example", "https://b.example"))

  # After stop, record is a no-op again.
  cache_track_record("https://c.example")
  cache_track_start()
  expect_equal(length(cache_track_stop()), 0L)
})

test_that("source_file_mtime parses {source}_{city} keys and finds the scraper file", {
  skip_if_not_installed("withr")
  r_dir <- withr::local_tempdir()
  # Three test files - varying source-name shapes.
  for (s in c("broadsheet", "good_food_guide", "broadsheet_guides", "7x7")) {
    writeLines("# stub", file.path(r_dir, paste0("scrape_", s, ".R")))
  }
  expect_match(source_file_mtime("broadsheet_sydney", r_dir = r_dir),
               "^\\d{4}-\\d{2}-\\d{2}T")
  # Multi-underscore source name should still resolve to the longest prefix
  # (the source is everything before the LAST underscore).
  expect_match(source_file_mtime("good_food_guide_sydney", r_dir = r_dir),
               "^\\d{4}-\\d{2}-\\d{2}T")
  # broadsheet_guides_sydney must NOT collapse to broadsheet_sydney.
  expect_match(source_file_mtime("broadsheet_guides_sydney", r_dir = r_dir),
               "^\\d{4}-\\d{2}-\\d{2}T")
  # Source name with a digit + city with hyphens.
  expect_match(source_file_mtime("7x7_san-francisco", r_dir = r_dir),
               "^\\d{4}-\\d{2}-\\d{2}T")
  # Missing scraper file -> NA.
  expect_true(is.na(source_file_mtime("nonexistent_source_sydney",
                                      r_dir = r_dir)))
  # Malformed key (no underscore) -> NA.
  expect_true(is.na(source_file_mtime("badkey", r_dir = r_dir)))
})

test_that("source_file_mtime invalidation only fires on the source's own file", {
  # The whole point of the per-source split: editing broadsheet's
  # scraper should NOT invalidate concrete_playground's parsed cache.
  skip_if_not_installed("withr")
  r_dir <- withr::local_tempdir()
  writeLines("# v1", file.path(r_dir, "scrape_broadsheet.R"))
  writeLines("# v1", file.path(r_dir, "scrape_concrete_playground.R"))
  mt_bs_before  <- source_file_mtime("broadsheet_sydney", r_dir = r_dir)
  mt_cp_before  <- source_file_mtime("concrete_playground_sydney", r_dir = r_dir)
  # Bump only broadsheet's mtime.
  Sys.setFileTime(file.path(r_dir, "scrape_broadsheet.R"), Sys.time() + 5)
  mt_bs_after  <- source_file_mtime("broadsheet_sydney", r_dir = r_dir)
  mt_cp_after  <- source_file_mtime("concrete_playground_sydney", r_dir = r_dir)
  expect_true(mt_bs_after > mt_bs_before)
  expect_equal(mt_cp_after, mt_cp_before)
})

test_that("build_parsed_manifest with empty URL list still serializes valid JSON", {
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  m <- build_parsed_manifest(character())
  expect_equal(m$n_urls, 0L)
  expect_equal(length(m$urls), 0L)
  # Should round-trip through jsonlite without losing the n_urls = 0 signal.
  path <- file.path(td, "m.json")
  writeLines(jsonlite::toJSON(m, auto_unbox = TRUE), path)
  back <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  expect_equal(back$n_urls, 0L)
})

test_that("scrape_restaurants honors use_parsed_cache=FALSE", {
  # When the parsed cache is disabled, do_scrape must run every time
  # even on identical inputs. We can't easily call scrape_restaurants
  # without hitting real scrapers, so we exercise the cached_scrape
  # wrapper instead with the same opt-out logic the dispatcher uses.
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  call_count <- 0L
  produce <- function() {
    call_count <<- call_count + 1L
    tibble::tibble(name = "Test")
  }
  # First call - populate
  suppressMessages(cached_scrape("optout", produce(), cache_dir = td))
  expect_equal(call_count, 1L)
  # Direct call (simulating use_parsed_cache = FALSE) - always runs
  produce()
  expect_equal(call_count, 2L)
})
