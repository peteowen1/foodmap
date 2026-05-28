devtools::load_all()

# 1. Drop South End from the cache
cache_path <- "cache/geocodes.csv"
if (file.exists(cache_path)) {
  cache <- utils::read.csv(cache_path, stringsAsFactors = FALSE,
                           na.strings = c("", "NA"))
  bad <- tolower(cache$name) == "south end"
  cat("Removing", sum(bad), "South End row(s) from cache\n")
  cache <- cache[!bad, , drop = FALSE]
  utils::write.csv(cache, cache_path, row.names = FALSE)
}

# 2. Run the pipeline up to dedup, then force-clear South End's lat/lng
#    (Broadsheet's API returned wrong coords for this venue) so the
#    address-aware geocode_restaurants() step picks fresh coords.
all <- scrape_all_sources("sydney", use_cache = TRUE) |>
  deduplicate_restaurants()

bad_se <- all$name == "South End"
cat("Wiping coords for", sum(bad_se), "South End row(s) before geocode\n")
all$latitude[bad_se]  <- NA_real_
all$longitude[bad_se] <- NA_real_

all <- all |>
  geocode_restaurants() |>
  harmonize_sources()

dir.create("output", showWarnings = FALSE)
export_csv(all, "output/sydney_all_sources.csv")
export_kml(all, "output/sydney_all_sources.kml", city = "sydney")
export_html(all, "docs/sydney.html",
            title = "foodmap - Sydney's hatted restaurants",
            city = "sydney")

# 3. Sanity check
se <- all[all$name == "South End", c("name", "suburb", "address",
                                      "latitude", "longitude")]
cat("\nSouth End after re-geocode:\n")
print(se, width = 200)
cat("Expected: ~-33.901, 151.182 (644 King St Erskineville)\n")
