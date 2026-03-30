# foodmap 0.3.0

## New features

* `scrape_all_sources(city)` scrapes every guide that supports a city and
  combines results with a `source` column identifying each guide.

* `deduplicate_restaurants()` fuzzy-matches venues across sources by
  normalized name + suburb, merging duplicates while keeping the richest
  metadata. Returns an `n_sources` column counting contributing guides.

* `use_cache` parameter on `create_food_map()`, `scrape_restaurants()`, and
  all HTTP-based scrapers. Caches responses in `cache/` for 24 hours —
  avoids re-fetching during development and debugging.

* KML export now groups placemarks into `<Folder>` elements by source when
  a `source` column is present, with color-coded pin styles per guide.
  Multi-source venues get a gold "multiple" pin.

* `create_food_map()` adds a `source` column to results for folder grouping.

## Bug fixes

* Fixed data-loss bug in Broadsheet RSC deduplication: venues without
 `profile_id` were silently discarded instead of kept.

* Fixed null pointer crash in AGFG detail page parsing when JSON-LD has no
  `geo` object.

* `create_food_map()` now validates city/source combination before scraping
  (was only validated deep inside individual scrapers).

## Improvements

* All 6 scrapers now report failure counts when individual items fail to parse,
  instead of silently returning partial results.

* `RATE_LIMIT_SECS` constant replaces hardcoded `Sys.sleep(0.2)` across all
  scrapers and the geocoder.

* `CUISINE_NAMES` constant in utils.R eliminates duplicated cuisine tag lists
  in the Good Food Guide scraper.

* Broadsheet API pagination response is now validated before trusting.

* Chromote page loads use `Page$loadEventFired(timeout = 10)` with fallback,
  instead of blind `Sys.sleep(5)`.

* Urban List suburb extraction now skips postcodes and state abbreviations.

* `export_csv()` wraps `write.csv()` with error handling.

* Roxygen `@return` annotations updated for all scrapers to include `rating`
  and `rating_scale` columns.

## Testing

* Added testthat test suite (169 tests): utils, geocode helpers, export
  functions, schema validation, and HTML fixture-based parser tests.

* GitHub Actions CI workflow runs `R CMD check` on push/PR to master.

# foodmap 0.2.0

* Initial multi-source scraper package with Broadsheet, Gourmet Traveller,
  Time Out, AGFG, Urban List, and Good Food Guide support.
