devtools::load_all()

# Drop the 5 NA-suburb GFG awards venues from the cache so they get
# re-geocoded against the new "Sydney" suburb hint
cache_path <- "cache/geocodes.csv"
bad_names <- c("Ibushi", "Omakase by Prefecture 48", "The White Horse",
               "Yan", "Yeodongsik")
if (file.exists(cache_path)) {
  cache <- utils::read.csv(cache_path, stringsAsFactors = FALSE,
                           na.strings = c("", "NA"))
  bad <- cache$name %in% bad_names
  cat("Removing", sum(bad), "row(s) from cache\n")
  cache <- cache[!bad, , drop = FALSE]
  utils::write.csv(cache, cache_path, row.names = FALSE)
}

# Run the Sydney pipeline; only the 5 cleared rows + any net-new
# venues will hit the API
all <- scrape_all_sources("sydney", use_cache = TRUE) |>
  deduplicate_restaurants()
clear <- all$name %in% bad_names
cat("Wiping coords for", sum(clear), "GFG-awards row(s) before geocode\n")
all$latitude[clear]  <- NA_real_
all$longitude[clear] <- NA_real_

all <- all |> geocode_restaurants() |> harmonize_sources()

dir.create("output", showWarnings = FALSE)
export_csv(all, "output/sydney_all_sources.csv")
export_kml(all, "output/sydney_all_sources.kml", city = "sydney")
export_html(all, "docs/sydney.html",
            title = "foodmap - Sydney's hatted restaurants",
            city = "sydney")

cat("\nNew coords for the previously-NA-suburb venues:\n")
print(all[all$name %in% bad_names,
          c("name", "suburb", "latitude", "longitude")], width = 200)
