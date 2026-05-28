# Run the country-fallback geocoder across every city map's CSV. Any
# row that's still NA after this either failed both bbox attempts
# (Google genuinely couldn't resolve it) or has no usable signal.
#
# For each city: loads the CSV, calls geocode_restaurants(city = city)
# so the new fallback kicks in for NA rows, then re-exports CSV / KML /
# HTML. Cache writes propagate so subsequent runs are cheap.
#
# Cost: at most (sum of missing-coord rows across cities) * 2 API calls,
# minus any cache hits. With the existing cache state we expect mostly
# cache fills, with a small handful of fresh API calls.

devtools::load_all()

text_cols <- c("name", "suburb", "address", "cuisine", "category",
               "source", "description", "url", "price_label",
               "cost_bracket", "rating_scale", "rating_label",
               "review_date", "neighborhood", "michelin_distinction",
               "formatted_address", "place_id")

cities <- list(
  list(city = "sydney",        csv = "output/sydney_all_sources.csv",
       kml = "output/sydney_all_sources.kml",
       html = "docs/sydney.html",
       title = "foodmap - Sydney's hatted restaurants"),
  list(city = "melbourne",     csv = "output/melbourne_all_sources.csv",
       kml = "output/melbourne_all_sources.kml",
       html = "docs/melbourne.html",
       title = "foodmap - Melbourne's best restaurants"),
  list(city = "san-francisco", csv = "output/san_francisco_all_sources.csv",
       kml = "output/san_francisco_all_sources.kml",
       html = "docs/san_francisco.html",
       title = "foodmap - San Francisco"),
  list(city = "honolulu",      csv = "output/honolulu_all_sources.csv",
       kml = "output/honolulu_all_sources.kml",
       html = "docs/honolulu.html",
       title = "foodmap - Honolulu"),
  list(city = "new-york",      csv = "output/new_york_all_sources.csv",
       kml = "output/new_york_all_sources.kml",
       html = "docs/new_york.html",
       title = "foodmap - New York"),
  list(city = "los-angeles",   csv = "output/los_angeles_all_sources.csv",
       kml = "output/los_angeles_all_sources.kml",
       html = "docs/los_angeles.html",
       title = "foodmap - Los Angeles"),
  list(city = "london",        csv = "output/london_all_sources.csv",
       kml = "output/london_all_sources.kml",
       html = "docs/london.html",
       title = "foodmap - London")
)

summary_rows <- list()

for (city in cities) {
  cat("\n=== ", city$city, " ===\n", sep = "")
  all <- tibble::as_tibble(utils::read.csv(
    city$csv, stringsAsFactors = FALSE, na.strings = c("", "NA")
  ))
  all$latitude  <- as.numeric(all$latitude)
  all$longitude <- as.numeric(all$longitude)
  for (col in intersect(text_cols, names(all))) {
    all[[col]] <- as.character(all[[col]])
  }

  before_missing <- sum(is.na(all$latitude) | is.na(all$longitude))
  all <- geocode_restaurants(all, city = city$city)
  after_missing <- sum(is.na(all$latitude) | is.na(all$longitude))

  utils::write.csv(all, city$csv, row.names = FALSE)
  export_kml(all, city$kml, city = city$city)
  export_html(all, city$html, title = city$title, city = city$city)

  summary_rows[[length(summary_rows) + 1]] <- data.frame(
    city = city$city,
    total = nrow(all),
    before_missing = before_missing,
    after_missing  = after_missing,
    resolved       = before_missing - after_missing
  )

  if (after_missing > 0) {
    cat("\nStill missing in ", city$city, ":\n", sep = "")
    still <- all[is.na(all$latitude) | is.na(all$longitude),
                 intersect(c("name", "suburb", "address", "source", "url"),
                           names(all))]
    print(still, n = Inf, width = 200)
  }
}

cat("\n=== Summary ===\n")
print(do.call(rbind, summary_rows), row.names = FALSE)
