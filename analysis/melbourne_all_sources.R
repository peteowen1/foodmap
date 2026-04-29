# Pull every Melbourne source into one combined map.
# Re-uses cached HTTP responses (24h) for scraping and the geocode cache
# at cache/geocodes.csv (managed by geocode_restaurants()).
#
# Note: the SMH Good Food Guide awards source (gfg_awards) is Sydney-only,
# so Melbourne pulls from 6 sources rather than 7.

devtools::load_all()

all <- scrape_all_sources("melbourne", use_cache = TRUE) |>
  deduplicate_restaurants() |>
  geocode_restaurants() |>
  infer_missing_price() |>
  harmonize_sources()
# To force re-geocoding (e.g. after suspecting stale coords):
#   geocode_restaurants(force_refresh = TRUE)

dir.create("output", showWarnings = FALSE)
export_csv(all, "output/melbourne_all_sources.csv")

if (any(!is.na(all$latitude))) {
  export_kml(all, "output/melbourne_all_sources.kml")
  # Interactive Leaflet HTML for GitHub Pages
  export_html(all, "docs/melbourne.html",
              title = "foodmap - Melbourne's best restaurants")
} else {
  cat("Skipped KML/HTML (no coordinates)\n")
}

cat("\nFinal counts:\n")
print(dplyr::count(all, n_sources, name = "venues"))
