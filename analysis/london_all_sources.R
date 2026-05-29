# London - pulls from five sources, the first UK city in foodmap:
#
#   - Time Out London - flagship Best Restaurants in London list
#   - The Infatuation London - hit list + category guides
#   - Eater London - Essential lists + category maps
#   - Michelin Guide London - greater-london path covers all stars,
#     Bib Gourmand and Selected
#   - World's 50 Best - the international tier picks for London
#
# country = "GB" + city = "london" bounds the geocoder to greater
# London (lat 51.28-51.69, lng -0.51 to 0.33). Brighton / Oxford /
# Bath venues that occasionally leak into "best UK" guides are
# filtered out at the bbox. No JBA equivalent for the UK is wired up
# yet - the closest analogue would be the Code Hospitality awards or
# the Estrella Damm Top 50 UK Restaurants, neither of which has been
# pulled in.

devtools::load_all()

all <- scrape_all_sources("london", use_cache = TRUE) |>
  deduplicate_restaurants() |>
  apply_manual_excludes(city = "london") |>
  geocode_restaurants(country = "GB", city = "london") |>
  infer_missing_price() |>
  harmonize_sources() |>
  assert_venue_count(city = "london")

dir.create("output", showWarnings = FALSE)
export_csv(all, "output/london_all_sources.csv")
export_diagnostics(all, "output/london_diagnostics.csv")

if (any(!is.na(all$latitude))) {
  export_kml(all, "output/london_all_sources.kml")
  export_html(all, "docs/london.html",
              title = "foodmap - London")
} else {
  cat("Skipped KML/HTML (no coordinates)\n")
}

cat("\nFinal counts:\n")
print(dplyr::count(all, n_sources, name = "venues"))
