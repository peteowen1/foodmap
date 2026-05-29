# Pull every Hobart source into one combined map.
# Sources for Hobart: Broadsheet + AGFG. Tasmania is geographically
# compact - the Hobart bbox covers the whole southern half of the
# state (Bruny Island, Coal River wine country, Mt Field).

devtools::load_all()

all <- scrape_all_sources("hobart", use_cache = TRUE) |>
  deduplicate_restaurants() |>
  apply_manual_excludes(city = "hobart") |>
  geocode_restaurants(country = "AU", city = "hobart") |>
  infer_missing_price() |>
  harmonize_sources() |>
  assert_venue_count(city = "hobart")

dir.create("output", showWarnings = FALSE)
export_csv(all, "output/hobart_all_sources.csv")
export_diagnostics(all, "output/hobart_diagnostics.csv")

if (any(!is.na(all$latitude))) {
  export_kml(all, "output/hobart_all_sources.kml")
  export_html(all, "docs/hobart.html",
              title = "foodmap - Hobart")
} else {
  cat("Skipped KML/HTML (no coordinates)\n")
}

cat("\nFinal counts:\n")
print(dplyr::count(all, n_sources, name = "venues"))
