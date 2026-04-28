# San Francisco — pulls from The Infatuation (their flagship "21
# Restaurants" guide). Pass `extra_guides` to scrape_infatuation()
# directly to broaden coverage across categories (best italian, best
# pizza, best ramen, etc.) when you want more venues.
#
# country = "US" tells the geocoder to bias Places API queries to the
# US bounding box and reject any cached coords outside that box.

devtools::load_all()

all <- scrape_all_sources("san-francisco", use_cache = TRUE) |>
  deduplicate_restaurants() |>
  geocode_restaurants(country = "US") |>
  harmonize_sources()

dir.create("output", showWarnings = FALSE)
export_csv(all, "output/san_francisco_all_sources.csv")

if (any(!is.na(all$latitude))) {
  export_kml(all, "output/san_francisco_all_sources.kml")
  export_html(all, "docs/san_francisco.html",
              title = "foodmap - San Francisco")
} else {
  cat("Skipped KML/HTML (no coordinates)\n")
}

cat("\nFinal counts:\n")
print(dplyr::count(all, n_sources, name = "venues"))
