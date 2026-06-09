# Refresh the committed Michelin snapshots used as a CI fallback.
#
# WHY THIS EXISTS
# ---------------
# Michelin's AWS WAF serves a 404/bot-challenge to GitHub Actions' cloud
# IP ranges, so scrape_michelin() reliably returns zero venues in CI even
# though the scraper code is correct (it returns 200 + full results from a
# residential IP). scrape_michelin() falls back to these committed parquet
# snapshots when the live scrape comes back empty.
#
# Run this locally (residential IP) whenever the Michelin Guide updates -
# in practice that's after each city's annual MICHELIN ceremony, so a
# refresh every month or two is plenty:
#
#   Rscript analysis/refresh_michelin_snapshot.R
#
# Then commit the changed inst/extdata/michelin_snapshots/*.parquet files.

suppressMessages(devtools::load_all(quiet = TRUE))

michelin_cities <- c("san-francisco", "new-york", "los-angeles", "london")
out_dir <- file.path("inst", "extdata", "michelin_snapshots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (city in michelin_cities) {
  cli::cli_h1("Snapshotting Michelin: {city}")
  tbl <- tryCatch(
    scrape_michelin(city, use_cache = FALSE),
    error = function(e) {
      cli::cli_warn("Skipped {city}: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(tbl) || nrow(tbl) == 0) {
    cli::cli_alert_danger("No venues scraped for {city} - snapshot NOT updated.")
    next
  }
  # RDS (not parquet) so the snapshot round-trips the tibble's exact
  # column types - same justification as the parsed cache in
  # R/cache_parsed.R, and it avoids adding arrow as a dependency just
  # for a fallback fixture.
  out <- file.path(out_dir, paste0(city, ".rds"))
  saveRDS(tbl, out)
  cli::cli_alert_success("Wrote {nrow(tbl)} venues -> {out}")
}
