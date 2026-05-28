# Rebuild the GitHub Pages HTML maps (docs/*.html) from the current
# output/*.csv exports, which already carry the widened-bbox regional
# coordinates. No re-scrape, no re-geocode - just export_html.

devtools::load_all()

# Belt-and-braces: make sure rmarkdown/htmlwidgets can find pandoc for
# selfcontained = TRUE (it is normally on PATH already).
if (Sys.getenv("RSTUDIO_PANDOC") == "" && !nzchar(Sys.which("pandoc"))) {
  Sys.setenv(
    RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
  )
}

cities <- list(
  list(city = "sydney",
       csv = "output/sydney_all_sources.csv",
       html = "docs/sydney.html",
       title = "foodmap - Sydney's hatted restaurants"),
  list(city = "melbourne",
       csv = "output/melbourne_all_sources.csv",
       html = "docs/melbourne.html",
       title = "foodmap - Melbourne's best restaurants"),
  list(city = "san-francisco",
       csv = "output/san_francisco_all_sources.csv",
       html = "docs/san_francisco.html",
       title = "foodmap - San Francisco"),
  list(city = "honolulu",
       csv = "output/honolulu_all_sources.csv",
       html = "docs/honolulu.html",
       title = "foodmap - Honolulu"),
  list(city = "new-york",
       csv = "output/new_york_all_sources.csv",
       html = "docs/new_york.html",
       title = "foodmap - New York"),
  list(city = "los-angeles",
       csv = "output/los_angeles_all_sources.csv",
       html = "docs/los_angeles.html",
       title = "foodmap - Los Angeles"),
  list(city = "london",
       csv = "output/london_all_sources.csv",
       html = "docs/london.html",
       title = "foodmap - London")
)

text_cols <- c("name", "suburb", "address", "cuisine", "category",
               "source", "description", "url", "price_label",
               "cost_bracket", "rating_scale", "rating_label",
               "review_date", "neighborhood", "michelin_distinction")

for (city in cities) {
  all <- tibble::as_tibble(utils::read.csv(
    city$csv, stringsAsFactors = FALSE, na.strings = c("", "NA")
  ))
  all$latitude  <- as.numeric(all$latitude)
  all$longitude <- as.numeric(all$longitude)
  for (col in intersect(text_cols, names(all))) {
    all[[col]] <- as.character(all[[col]])
  }
  export_html(all, city$html, title = city$title, city = city$city)
}
