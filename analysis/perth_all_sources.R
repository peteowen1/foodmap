# Pull every Perth source into one combined map.
# Sources for Perth: Broadsheet + AGFG. Perth and Margaret River are
# AGFG's strongest WA coverage; Broadsheet's Perth hotlist rounds it out.

devtools::load_all()

all <- scrape_all_sources("perth", use_cache = TRUE) |>
  deduplicate_restaurants() |>
  apply_manual_excludes(city = "perth") |>
  geocode_restaurants(country = "AU", city = "perth") |>
  infer_missing_price() |>
  harmonize_sources()

dir.create("output", showWarnings = FALSE)
export_csv(all, "output/perth_all_sources.csv")
export_diagnostics(all, "output/perth_diagnostics.csv")

if (any(!is.na(all$latitude))) {
  export_kml(all, "output/perth_all_sources.kml")
  export_html(all, "docs/perth.html",
              title = "foodmap - Perth")
} else {
  cat("Skipped KML/HTML (no coordinates)\n")
}

cat("\nFinal counts:\n")
print(dplyr::count(all, n_sources, name = "venues"))
