# Test Aalia dedup fix
devtools::load_all()

d <- read.csv("output/sydney_all_sources.csv", stringsAsFactors = FALSE) |>
  tibble::as_tibble()

deduped <- deduplicate_restaurants(d)

# Find Aalia in deduped results
aalia <- deduped[grepl("aalia", deduped$name, ignore.case = TRUE), ]
cat("Aalia entries after dedup:\n")
for (i in seq_len(nrow(aalia))) {
  r <- aalia[i, ]
  cat(sprintf("  %s | sources: %s | n_sources: %d\n",
    r$name, r$source, r$n_sources))
}
