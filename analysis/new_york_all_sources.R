# New York City - pulls from eight sources covering critic picks,
# editorial roundups, monthly hit lists, and chef awards:
#
#   - Time Out NYC - flagship 100 Best
#   - The Infatuation NYC - hit list + category guides
#   - Eater NY - Essential 38 + category maps
#   - Michelin Guide NYC - all stars + Bib Gourmand + Selected
#   - Resy NYC - monthly Hit List with structured coords + cuisine
#   - Bon Appétit - annual Best New Restaurants in America (NYC subset)
#   - World's 50 Best - international tier (NYC entries only)
#   - James Beard Awards - hand-curated Best Chef: New York State + classics
#
# country = "US" + city = "new-york" tells the geocoder to bias the
# Places API to the five boroughs + immediate commute (lat 40.49-40.92,
# lng -74.28 to -73.68). Westchester / Hudson Valley / Long Island
# venues that leak through guide rosters are filtered out at the bbox.

devtools::load_all()

all <- scrape_all_sources("new-york", use_cache = TRUE) |>
  deduplicate_restaurants() |>
  geocode_restaurants(country = "US", city = "new-york",
                      migrate_neighborhoods = TRUE) |>
  infer_missing_price() |>
  harmonize_sources()

dir.create("output", showWarnings = FALSE)
export_csv(all, "output/new_york_all_sources.csv")

if (any(!is.na(all$latitude))) {
  export_kml(all, "output/new_york_all_sources.kml")
  export_html(all, "docs/new_york.html",
              title = "foodmap - New York")
} else {
  cat("Skipped KML/HTML (no coordinates)\n")
}

cat("\nFinal counts:\n")
print(dplyr::count(all, n_sources, name = "venues"))
