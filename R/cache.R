#' Read cached HTML response for a URL
#' @param url Character. The URL that was fetched.
#' @param max_age_hours Numeric. Maximum cache age in hours. Default 24.
#' @param cache_dir Character. Cache directory path. Default "cache".
#' @return Cached HTML string, or NULL if not cached or expired.
#' @noRd
cache_get <- function(url, max_age_hours = 24, cache_dir = "cache") {
  path <- cache_path(url, cache_dir)
  if (!file.exists(path)) return(NULL)

  age_hours <- difftime(Sys.time(), file.mtime(path), units = "hours")
  if (as.numeric(age_hours) > max_age_hours) return(NULL)

  readLines(path, warn = FALSE) |> paste(collapse = "\n")
}

#' Save HTML response to cache
#' @param url Character. The URL that was fetched.
#' @param html Character. The HTML response body.
#' @param cache_dir Character. Cache directory path. Default "cache".
#' @noRd
cache_set <- function(url, html, cache_dir = "cache") {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  writeLines(html, cache_path(url, cache_dir))
  invisible(NULL)
}

#' Build cache file path from URL
#' @noRd
cache_path <- function(url, cache_dir = "cache") {
  hash <- substr(rlang::hash(url), 1, 16)
  file.path(cache_dir, paste0(hash, ".html"))
}
