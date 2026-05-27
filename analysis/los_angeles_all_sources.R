# Los Angeles - pulls from eight sources mirroring the NYC stack:
#
#   - Time Out LA
#   - The Infatuation LA
#   - Eater LA - Essential 38 + category maps
#   - Michelin Guide LA - covered under the California regional guide
#   - Resy LA - monthly Hit List with structured coords + cuisine
#   - Bon Appétit - annual Best New (LA subset)
#   - World's 50 Best - LA / Beverly Hills / Santa Monica entries
#   - James Beard Awards - hand-curated Best Chef: California + classics
#
# country = "US" + city = "los-angeles" bounds the geocoder to LA
# proper, Santa Monica, Venice, Pasadena, Culver City, Beverly Hills,
# Hollywood and the immediate San Gabriel Valley (lat 33.85-34.30,
# lng -118.70 to -118.10). Long Beach / Orange County venues that
# leak in from city guides are filtered at the bbox.

devtools::load_all()

all <- scrape_all_sources("los-angeles", use_cache = TRUE) |>
  deduplicate_restaurants() |>
  geocode_restaurants(country = "US", city = "los-angeles",
                      migrate_neighborhoods = TRUE) |>
  infer_missing_price() |>
  harmonize_sources()

dir.create("output", showWarnings = FALSE)
export_csv(all, "output/los_angeles_all_sources.csv")

if (any(!is.na(all$latitude))) {
  export_kml(all, "output/los_angeles_all_sources.kml")
  export_html(all, "docs/los_angeles.html",
              title = "foodmap - Los Angeles")
} else {
  cat("Skipped KML/HTML (no coordinates)\n")
}

cat("\nFinal counts:\n")
print(dplyr::count(all, n_sources, name = "venues"))
