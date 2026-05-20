# Re-geocode regional venues that the old tight metro bounding boxes
# rejected, then regenerate every export. The boxes in R/region.R are
# now ~4h drive (Sydney, Melbourne) / ~2h drive (San Francisco), so
# venues down the NSW South Coast, along the Great Ocean Road and around
# Napa / Monterey survive geocoding and land on the map.
#
# Works from the existing output/*.csv (no re-scrape): only venues still
# missing coordinates hit the Places API.

devtools::load_all()

cities <- list(
  list(slug = "sydney", country = "AU",
       csv  = "output/sydney_all_sources.csv",
       kml  = "output/sydney_all_sources.kml",
       html = "docs/sydney.html",
       title = "foodmap - Sydney's hatted restaurants",
       # old tight metro box, used only to flag what's newly in range
       old  = list(lat = c(-34.20, -33.50), lng = c(150.50, 151.40))),
  list(slug = "melbourne", country = "AU",
       csv  = "output/melbourne_all_sources.csv",
       kml  = "output/melbourne_all_sources.kml",
       html = "docs/melbourne.html",
       title = "foodmap - Melbourne's best restaurants",
       old  = list(lat = c(-38.20, -37.55), lng = c(144.50, 145.50))),
  list(slug = "san-francisco", country = "US",
       csv  = "output/san_francisco_all_sources.csv",
       kml  = "output/san_francisco_all_sources.kml",
       html = "docs/san_francisco.html",
       title = "foodmap - San Francisco",
       old  = list(lat = c(37.20, 38.20), lng = c(-123.10, -121.80)))
)

for (city in cities) {
  cli::cli_h1(city$slug)

  all <- tibble::as_tibble(utils::read.csv(
    city$csv, stringsAsFactors = FALSE, na.strings = c("", "NA")
  ))
  all$latitude  <- as.numeric(all$latitude)
  all$longitude <- as.numeric(all$longitude)
  # read.csv types an all-NA column as logical; force the text columns
  # back to character so the geocode cache join (and the exporters)
  # don't hit a logical-vs-character type clash.
  text_cols <- c("name", "suburb", "address", "cuisine", "category",
                 "source", "description", "url", "price_label",
                 "cost_bracket", "rating_scale", "rating_label",
                 "review_date", "neighborhood", "michelin_distinction")
  for (col in intersect(text_cols, names(all))) {
    all[[col]] <- as.character(all[[col]])
  }

  missing_before <- sum(is.na(all$latitude))
  all <- geocode_restaurants(all, country = city$country, city = city$slug)
  missing_after <- sum(is.na(all$latitude))

  # Venues that now have coordinates but sit outside the old metro box:
  # the regional places this fix is about.
  o <- city$old
  regional <- !is.na(all$latitude) &
    (all$latitude  < o$lat[1] | all$latitude  > o$lat[2] |
     all$longitude < o$lng[1] | all$longitude > o$lng[2])

  cli::cli_alert_success(
    "{city$slug}: geocoded {missing_before - missing_after}/{missing_before} missing coords"
  )
  if (any(regional)) {
    cli::cli_alert_info(
      "{sum(regional)} venue{?s} now mapped outside the old metro box:"
    )
    print(all[regional, c("name", "suburb", "latitude", "longitude")],
          n = sum(regional))
  }

  export_csv(all, city$csv)
  export_kml(all, city$kml)
  export_html(all, city$html, title = city$title)
}

cli::cli_alert_success("All three maps regenerated.")
