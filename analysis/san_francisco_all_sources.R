# San Francisco — pulls from The Infatuation (their flagship "21
# Restaurants" guide). Pass `extra_guides` to scrape_infatuation()
# directly to broaden coverage across categories (best italian, best
# pizza, best ramen, etc.) when you want more venues.
#
# country = "US" tells the geocoder to bias Places API queries to the
# US bounding box and reject any cached coords outside that box.

devtools::load_all()

# michelin skipped: scraper aborts the R process mid-parse on this city's
# Guide (~357 detail pages). Diagnose separately; revisit when fixed.
all <- scrape_all_sources("san-francisco", use_cache = TRUE,
                          skip_sources = "michelin") |>
  deduplicate_restaurants() |>
  geocode_restaurants(country = "US", city = "san-francisco") |>
  infer_missing_price() |>
  harmonize_sources()

dir.create("output", showWarnings = FALSE)
export_csv(all, "output/san_francisco_all_sources.csv")
export_diagnostics(all, "output/san_francisco_diagnostics.csv")

if (any(!is.na(all$latitude))) {
  export_kml(all, "output/san_francisco_all_sources.kml")
  export_html(all, "docs/san_francisco.html",
              title = "foodmap - San Francisco")
} else {
  cat("Skipped KML/HTML (no coordinates)\n")
}

cat("\nFinal counts:\n")
print(dplyr::count(all, n_sources, name = "venues"))
