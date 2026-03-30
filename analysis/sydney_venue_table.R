# Sydney venue summary table — run with: Rscript analysis/sydney_venue_table.R
devtools::load_all()

d <- read.csv("output/sydney_all_sources.csv", stringsAsFactors = FALSE) |>
  tibble::as_tibble()

deduped <- deduplicate_restaurants(d)

# Select the most useful columns for browsing
summary_tbl <- deduped |>
  dplyr::select(
    name, suburb, cuisine, category,
    source, n_sources,
    price_range, rating, rating_scale,
    latitude, longitude,
    description, url
  ) |>
  dplyr::arrange(dplyr::desc(n_sources), name)

cat("Sydney venues:", nrow(summary_tbl), "\n")
cat("With coordinates:", sum(!is.na(summary_tbl$latitude)), "\n")
cat("Multi-source:", sum(summary_tbl$n_sources > 1), "\n\n")

# Save as CSV for easy browsing in Excel/Sheets
outpath <- "output/sydney_venue_summary.csv"
write.csv(summary_tbl, outpath, row.names = FALSE)
cat("Written to:", outpath, "\n\n")

# Print top 20 (most-recommended venues)
cat("Top 20 most-recommended venues:\n")
top <- head(summary_tbl, 20)
for (i in seq_len(nrow(top))) {
  r <- top[i, ]
  cat(sprintf(
    "  %2d. %-30s | %-15s | %-20s | sources: %d | %s\n",
    i,
    substr(r$name, 1, 30),
    ifelse(is.na(r$suburb), "—", substr(r$suburb, 1, 15)),
    ifelse(is.na(r$cuisine), "—", substr(r$cuisine, 1, 20)),
    r$n_sources,
    r$source
  ))
}
