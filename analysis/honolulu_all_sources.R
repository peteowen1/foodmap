# Honolulu / Oʻahu - pulls from five sources covering critic picks,
# magazine editorial, reader-voted awards and chef awards:
#
#   - The Infatuation (Oʻahu) - flagship best restaurants + best bars
#   - Thrillist Honolulu - "Best Restaurants in Honolulu Right Now"
#   - HONOLULU Magazine - rolling "Best Restaurants in Honolulu" plus
#       the current annual "Best New Restaurants" cohort
#   - Hale ʻAina Awards - HONOLULU Magazine's annual reader-voted
#       awards (Gold / Silver / Bronze / Finalist across ~40 categories)
#   - James Beard Awards - hand-curated Hawaiʻi recognitions (Best Chef:
#       Northwest & Pacific + America's Classics)
#
# country = "US" + city = "honolulu" tells the geocoder to bias the
# Places API to Oʻahu (lat 21.20-21.78, lng -158.35 to -157.60) and
# reject any cached coords outside that box. Outer-island venues that
# leak through (Maui / Big Island / Kauaʻi recommendations from any
# guide) are silently dropped at the bbox check.

devtools::load_all()

all <- scrape_all_sources("honolulu", use_cache = TRUE) |>
  deduplicate_restaurants() |>
  apply_manual_excludes(city = "honolulu") |>
  geocode_restaurants(country = "US", city = "honolulu") |>
  infer_missing_price() |>
  harmonize_sources() |>
  assert_venue_count(city = "honolulu")

dir.create("output", showWarnings = FALSE)
export_csv(all, "output/honolulu_all_sources.csv")
export_diagnostics(all, "output/honolulu_diagnostics.csv")

if (any(!is.na(all$latitude))) {
  export_kml(all, "output/honolulu_all_sources.kml")
  export_html(all, "docs/honolulu.html",
              title = "foodmap - Honolulu")
} else {
  cat("Skipped KML/HTML (no coordinates)\n")
}

cat("\nFinal counts:\n")
print(dplyr::count(all, n_sources, name = "venues"))
