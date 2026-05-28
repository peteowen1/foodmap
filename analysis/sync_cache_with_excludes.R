# Cache hygiene: drop cache rows whose (name, city) matches a row in
# inst/extdata/manual_excludes.csv. Mostly removes the legacy
# cross-country pollution (Australian "Leila" etc. that were
# matching Honolulu rows via the NA-NA suburb collision before the
# country pre-filter shipped) plus any other manually-excluded
# venue's stale entry.
#
# Rerun whenever you add a new entry to manual_excludes.csv and want
# to keep the cache tidy. Safe to re-run - missing matches are
# silently no-ops.
#
# Caveats:
#   - Drops by (name) alone within the relevant city's country. Doesn't
#     attempt suburb-level surgery because the polluted rows have NA
#     suburb anyway; a more selective tool would just be a cache row
#     editor.
#   - The drop is permanent on disk. Re-geocoding to restore would
#     cost an API call per dropped venue.

devtools::load_all()

cache_path <- "cache/geocodes.csv"
excludes_path <- system.file("extdata", "manual_excludes.csv",
                             package = "foodmap")

cache <- utils::read.csv(cache_path, stringsAsFactors = FALSE,
                         na.strings = "NA")
excludes <- utils::read.csv(excludes_path, stringsAsFactors = FALSE,
                            na.strings = c("", "NA"))

cat("Cache rows before:", nrow(cache), "\n")
cat("Exclude entries  :", nrow(excludes), "\n\n")

# For each exclude, find matching cache rows (by lowercase name) where
# the cache coords sit in the EXCLUDE's city's country. That way an
# exclude scoped to "honolulu" (US) won't accidentally drop an AU-coord
# row in the cache that's actually serving a different city.
drop_mask <- rep(FALSE, nrow(cache))
for (i in seq_len(nrow(excludes))) {
  city_slug <- excludes$city[i]
  country <- if (city_slug == "*") NA_character_ else city_country(city_slug)

  name_hit <- !is.na(cache$name) &
    tolower(cache$name) == tolower(excludes$name[i])

  if (!is.na(country) && !is.null(country)) {
    in_target <- is_in_country(cache$latitude, cache$longitude, country)
    cell_hit <- name_hit & in_target
  } else {
    cell_hit <- name_hit
  }

  if (any(cell_hit)) {
    cat(sprintf("  - %s (city %s): %d cache row(s) to drop\n",
                excludes$name[i], city_slug, sum(cell_hit)))
  }
  drop_mask <- drop_mask | cell_hit
}

# Conversely: the pollution case is when the cache row's country
# DOESN'T match the exclude's city's country. Catch those explicitly.
# Example: cache has "Leila NA" with Australian coords; exclude is for
# Honolulu (US). The check above would NOT drop it. But the row is
# still legacy pollution we want gone.
for (i in seq_len(nrow(excludes))) {
  city_slug <- excludes$city[i]
  if (city_slug == "*") next
  country <- city_country(city_slug)
  if (is.na(country) || is.null(country)) next

  name_hit <- !is.na(cache$name) &
    tolower(cache$name) == tolower(excludes$name[i])
  out_of_country <- !is_in_country(cache$latitude, cache$longitude, country)
  legacy <- name_hit & out_of_country
  if (any(legacy)) {
    cat(sprintf("  - %s (legacy pollution, foreign coords): %d cache row(s) to drop\n",
                excludes$name[i], sum(legacy)))
    drop_mask <- drop_mask | legacy
  }
}

if (!any(drop_mask)) {
  cat("\nNo cache entries match. Nothing to do.\n")
} else {
  cat("\nTotal cache rows to drop:", sum(drop_mask), "\n")
  cache <- cache[!drop_mask, , drop = FALSE]
  utils::write.csv(cache, cache_path, row.names = FALSE)
  cat("Cache rows after :", nrow(cache), "\n")
}
