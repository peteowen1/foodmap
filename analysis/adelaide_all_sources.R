# Pull every Adelaide source into one combined map.
# Sources for Adelaide: Broadsheet + AGFG. Broadsheet covers South
# Australia at parity with the eastern cities; AGFG's SA awards page
# is the structured-data backbone.

devtools::load_all()

all <- scrape_all_sources("adelaide", use_cache = TRUE) |>
  deduplicate_restaurants() |>
  apply_manual_excludes(city = "adelaide") |>
  geocode_restaurants(country = "AU", city = "adelaide") |>
  infer_missing_price() |>
  harmonize_sources()

dir.create("output", showWarnings = FALSE)
export_csv(all, "output/adelaide_all_sources.csv")
export_diagnostics(all, "output/adelaide_diagnostics.csv")

if (any(!is.na(all$latitude))) {
  export_kml(all, "output/adelaide_all_sources.kml")
  export_html(all, "docs/adelaide.html",
              title = "foodmap - Adelaide")
} else {
  cat("Skipped KML/HTML (no coordinates)\n")
}

cat("\nFinal counts:\n")
print(dplyr::count(all, n_sources, name = "venues"))
