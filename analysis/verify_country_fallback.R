# Force the country-only fallback to fire by clearing one regional
# Sydney venue from both the cache and the CSV, then watching it
# resolve. Unlike regeocode_all_cities.R (which is dominated by cache
# hits), this targets the actual API code path.
#
# Target: "Pipit" in Pottsville (Northern Rivers, ~-28.4, 153.6) -
# legitimately listed in the Sydney Good Food Guide but lat ~-28 is
# well outside Sydney's bbox (lat -36.6 to -31). The first city-bbox
# attempt should miss; the country-only fallback should resolve.
#
# Cost: ~2 API calls (city attempt + country fallback).

devtools::load_all()

target_name   <- "Pipit"
target_suburb <- "Pottsville"

# 1. Strip target from cache so cache_apply can't short-circuit.
cache_path <- "cache/geocodes.csv"
cache <- utils::read.csv(cache_path, stringsAsFactors = FALSE,
                         na.strings = "NA")
# Use %in% rather than ==/&: equality propagates NA, which would silently
# drop any NA-name cache rows when negated in the subset below.
hit <- (cache$name %in% target_name) & (cache$suburb %in% target_suburb)
cat("Cache had", sum(hit), "row(s) for", target_name, "/",
    target_suburb, "- evicting\n")
cache <- cache[!hit, , drop = FALSE]
utils::write.csv(cache, cache_path, row.names = FALSE)

# 2. Load Sydney CSV, NA the target's coords so the geocode loop hits it.
csv_path <- "output/sydney_all_sources.csv"
all <- tibble::as_tibble(utils::read.csv(
  csv_path, stringsAsFactors = FALSE, na.strings = c("", "NA")
))
all$latitude  <- as.numeric(all$latitude)
all$longitude <- as.numeric(all$longitude)
text_cols <- c("name", "suburb", "address", "cuisine", "category",
               "source", "description", "url", "price_label",
               "cost_bracket", "rating_scale", "rating_label",
               "review_date", "neighborhood", "michelin_distinction",
               "formatted_address", "place_id")
for (col in intersect(text_cols, names(all))) {
  all[[col]] <- as.character(all[[col]])
}

target_idx <- which(all$name == target_name & all$suburb == target_suburb)
cat("Found", length(target_idx), "row(s) in CSV - clearing coords\n")
all$latitude[target_idx]  <- NA_real_
all$longitude[target_idx] <- NA_real_
all$formatted_address[target_idx] <- NA_character_
all$place_id[target_idx]          <- NA_character_

# 3. Geocode. Should make exactly one API attempt (city) that misses,
# then a fallback attempt (country) that hits.
all <- geocode_restaurants(all, city = "sydney")

# 4. Report the resolved coords.
resolved <- all[target_idx, c("name", "suburb", "latitude", "longitude",
                              "formatted_address")]
cat("\nResolved venue:\n")
print(resolved, width = 200)

if (any(is.na(resolved$latitude))) {
  stop("Country fallback failed - target still has no coords")
}
cat("\n[OK] Country fallback resolved the target via a fresh API call.\n")
