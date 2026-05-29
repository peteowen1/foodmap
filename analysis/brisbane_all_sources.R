# Pull every Brisbane source into one combined map.
# Brisbane is supported by Broadsheet + AGFG. broadsheet_guides and the
# editorial cafe/bar sources only ship in syd/mel right now, so the
# southeast Queensland mix leans more restaurant than the Sydney /
# Melbourne pulls until those sources extend.

devtools::load_all()

all <- scrape_all_sources("brisbane", use_cache = TRUE) |>
  deduplicate_restaurants() |>
  apply_manual_excludes(city = "brisbane") |>
  geocode_restaurants(country = "AU", city = "brisbane") |>
  infer_missing_price() |>
  harmonize_sources()

dir.create("output", showWarnings = FALSE)
export_csv(all, "output/brisbane_all_sources.csv")
export_diagnostics(all, "output/brisbane_diagnostics.csv")

if (any(!is.na(all$latitude))) {
  export_kml(all, "output/brisbane_all_sources.kml")
  export_html(all, "docs/brisbane.html",
              title = "foodmap - Brisbane")
} else {
  cat("Skipped KML/HTML (no coordinates)\n")
}

cat("\nFinal counts:\n")
print(dplyr::count(all, n_sources, name = "venues"))
