# Pull the SMH Good Food Guide 2026 hat winners into a KML + CSV.
# Source: https://archive.is/z1iVx (SMH, 13 October 2025)
#
# Geocoding is automatically cached by geocode_restaurants() to
# cache/geocodes.csv — re-runs are free unless the venue list changes.

devtools::load_all()

# 1. Load the curated 2026 awards list (146 venues across 1, 2, 3 hats)
awards <- scrape_gfg_awards("sydney")
cat("Loaded", nrow(awards), "hatted venues\n")
print(dplyr::count(awards, hats))

# 2. Geocode (skipped if GOOGLE_PLACES_API_KEY is not set)
api_key <- Sys.getenv("GOOGLE_PLACES_API_KEY")
if (nzchar(api_key)) {
  awards <- geocode_restaurants(awards)
  cat("Geocoded:", sum(!is.na(awards$latitude)), "/", nrow(awards), "\n")
} else {
  message(
    "GOOGLE_PLACES_API_KEY not set — skipping geocode. ",
    "CSV will still be written; KML needs coordinates so it will be skipped. ",
    "Set the key with Sys.setenv(GOOGLE_PLACES_API_KEY = \"...\") and re-run ",
    "to produce the KML."
  )
}

# 3. Export to output/ (CSV always; KML only if we have coordinates)
dir.create("output", showWarnings = FALSE)
export_csv(awards, "output/sydney_gfg_awards_2026.csv")
cat("Wrote output/sydney_gfg_awards_2026.csv\n")

if (any(!is.na(awards$latitude))) {
  export_kml(awards, "output/sydney_gfg_awards_2026.kml")
  cat("Wrote output/sydney_gfg_awards_2026.kml\n")
} else {
  cat("Skipped KML (no coordinates yet — set GOOGLE_PLACES_API_KEY and re-run)\n")
}
