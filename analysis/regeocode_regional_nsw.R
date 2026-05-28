# Verifier for the geocoder's country-only fallback.
#
# Reads the existing Sydney CSV (which has 23 rows with NA coords - all
# regional NSW venues that failed the strict Sydney bbox geocode), runs
# geocode_restaurants() with city = "sydney" so the new fallback kicks
# in only for the NA rows, and re-exports CSV / KML / HTML so the
# Sydney map stays in sync. Cache gets updated as a side effect, so
# the result persists across sessions.
#
# Cost: at most 23 venues * 2 API attempts each, in practice ~30-40
# Text Search calls.

devtools::load_all()

csv_path <- "output/sydney_all_sources.csv"
all <- tibble::as_tibble(utils::read.csv(
  csv_path, stringsAsFactors = FALSE, na.strings = c("", "NA")
))
all$latitude  <- as.numeric(all$latitude)
all$longitude <- as.numeric(all$longitude)
# read.csv infers all-NA columns as logical; the cache+API code paths
# assume these are character, so coerce upfront (same pattern as
# regen_docs_html.R).
text_cols <- c("name", "suburb", "address", "cuisine", "category",
               "source", "description", "url", "price_label",
               "cost_bracket", "rating_scale", "rating_label",
               "review_date", "neighborhood", "michelin_distinction",
               "formatted_address", "place_id")
for (col in intersect(text_cols, names(all))) {
  all[[col]] <- as.character(all[[col]])
}

before_missing <- all$name[is.na(all$latitude) | is.na(all$longitude)]
cat("Before: ", sum(!is.na(all$latitude)), "/", nrow(all),
    "have coords. Missing names:\n")
cat(paste0("  - ", before_missing, "\n"), sep = "")

all <- geocode_restaurants(all, city = "sydney")

still_missing <- all$name[is.na(all$latitude) | is.na(all$longitude)]
got_fixed <- setdiff(before_missing, still_missing)

cat("\nAfter:  ", sum(!is.na(all$latitude)), "/", nrow(all),
    "have coords. Resolved", length(got_fixed), "of",
    length(before_missing), "regional venues.\n\n")

if (length(got_fixed) > 0) {
  cat("Newly-geocoded venues:\n")
  print(all[all$name %in% got_fixed,
            c("name", "suburb", "latitude", "longitude")],
        n = Inf, width = 200)
}

if (length(still_missing) > 0) {
  cat("\nStill missing:\n")
  print(all[all$name %in% still_missing,
            c("name", "suburb", "address", "url")],
        n = Inf, width = 200)
}

# Persist the enriched data so docs / KML stay in sync. CSV keeps every
# venue (raw geocoded data); KML + HTML apply the Sydney bbox filter to
# drop the regional venues from the map but keep them in the dataset.
utils::write.csv(all, csv_path, row.names = FALSE)
export_kml(all, "output/sydney_all_sources.kml", city = "sydney")
export_html(all, "docs/sydney.html",
            title = "foodmap - Sydney's hatted restaurants",
            city = "sydney")
