# Sydney union table — one row per venue per source
# Run: Rscript analysis/sydney_union_table.R
devtools::load_all()

d <- read.csv("output/sydney_all_sources.csv", stringsAsFactors = FALSE) |>
  tibble::as_tibble()

harmonized <- harmonize_sources(d)

cat("Rows:", nrow(harmonized), "\n")
cat("Columns:", paste(names(harmonized), collapse = ", "), "\n\n")

# Coverage summary per source
for (src in unique(harmonized$source)) {
  rows <- harmonized[harmonized$source == src, ]
  cat(src, "(", nrow(rows), "venues ):\n")
  has_cuisine <- sum(!is.na(rows$cuisine))
  has_price <- sum(!is.na(rows$price_label))
  has_cost <- sum(!is.na(rows$cost_bracket))
  has_rating <- sum(!is.na(rows$rating_label))
  has_coords <- sum(!is.na(rows$latitude))
  has_desc <- sum(!is.na(rows$description) & rows$description != "")
  cat("  cuisine:", has_cuisine, "| price:", has_price,
      "| cost:", has_cost, "| rating:", has_rating,
      "| coords:", has_coords, "| desc:", has_desc, "\n")
}

# Show a few multi-source venues
cat("\nSample: Cafe Paci across sources\n")
paci <- harmonized[grepl("Cafe Paci|Caf\u00e9 Paci", harmonized$name, ignore.case = TRUE), ]
for (i in seq_len(nrow(paci))) {
  r <- paci[i, ]
  cat(sprintf("  %-20s | %-15s | %-12s | %-10s | %-12s | coords: %s\n",
    r$source,
    ifelse(is.na(r$cuisine), "—", substr(r$cuisine, 1, 15)),
    ifelse(is.na(r$price_label), "—", r$price_label),
    ifelse(is.na(r$cost_bracket), "—", r$cost_bracket),
    ifelse(is.na(r$rating_label), "—", r$rating_label),
    ifelse(is.na(r$latitude), "no", "yes")
  ))
}

outpath <- "output/sydney_all_venues_union.csv"
write.csv(harmonized, outpath, row.names = FALSE)
cat("\nWritten to:", outpath, "\n")
