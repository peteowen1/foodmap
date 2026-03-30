# Check what columns each source actually populates
d <- read.csv("output/sydney_all_sources.csv", stringsAsFactors = FALSE)

for (src in unique(d$source)) {
  rows <- d[d$source == src, ]
  cat(src, ":", nrow(rows), "rows\n")
  for (col in names(rows)) {
    non_na <- sum(!is.na(rows[[col]]) & rows[[col]] != "")
    if (non_na > 0) cat("  ", col, "=", non_na, "/", nrow(rows), "\n")
  }
  cat("\n")
}
