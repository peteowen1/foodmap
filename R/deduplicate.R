#' Deduplicate restaurants across sources
#'
#' Merges duplicate venues that appear in multiple guides. Matches on
#' normalized name + suburb, then keeps the richest metadata from each copy.
#'
#' @param restaurants A tibble with a `source` column, as returned by
#'   [scrape_all_sources()].
#'
#' @return A tibble with duplicates merged. The `source` column shows all
#'   contributing sources (e.g., "broadsheet, timeout"). A new `n_sources`
#'   column counts how many guides listed the venue.
#' @export
deduplicate_restaurants <- function(restaurants) {
  if (!"source" %in% names(restaurants)) {
    cli::cli_warn("No {.field source} column found, nothing to deduplicate.")
    return(restaurants)
  }

  cli::cli_h2("Deduplicating {nrow(restaurants)} venues")

  # Normalize name for matching:
  #   - lowercase, normalize curly apostrophes
  #   - normalize "&" and "+" to "and" (handles "Bessie's & Alma's"
  #     vs "Bessie's and Alma's")
  #   - collapse street abbreviations (Street/St, Road/Rd, Avenue/Ave)
  #   - strip " restaurant" anywhere except at start of name
  #     ("Bathers' Pavilion Restaurant" -> "Bathers' Pavilion",
  #      "Bentley Restaurant & Bar" -> "Bentley and Bar";
  #      "Restaurant Hubert" stays unchanged)
  #   - strip trailing cuisine word ("Abhi's Indian" -> "Abhi's")
  #   - strip non-alphanumeric, squish whitespace
  cuisine_re <- paste(
    c("indian", "italian", "japanese", "chinese", "thai", "korean",
      "vietnamese", "greek", "mexican", "mediterranean", "seafood",
      "french", "spanish", "turkish", "lebanese", "modern australian",
      "contemporary", "asian", "european", "middle eastern"),
    collapse = "|"
  )
  restaurants$name_norm <- restaurants$name |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower() |>
    stringr::str_replace_all("\\s*[&+]\\s*", " and ") |>
    stringr::str_replace_all("\\bstreet\\b", "st") |>
    stringr::str_replace_all("\\broad\\b", "rd") |>
    stringr::str_replace_all("\\bavenue\\b", "ave") |>
    stringr::str_replace_all("\\btown hall\\b", "townhall") |>
    stringr::str_replace_all("\\s+the\\s+", " ") |>
    stringr::str_replace_all("\\s+restaurant\\b", "") |>
    stringr::str_remove(paste0("\\s+(", cuisine_re, ")$")) |>
    stringr::str_remove("\\s+(sydney|melbourne|brisbane|adelaide|perth|hobart|canberra|darwin)$") |>
    stringr::str_remove_all("[^a-z0-9 ]") |>
    stringr::str_squish()

  # Normalize suburb - strip common suffixes so "Sydney CBD" matches "Sydney"
  # and "Campbell ACT" matches "Campbell"
  restaurants$suburb_norm <- restaurants$suburb |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower() |>
    stringr::str_squish() |>
    stringr::str_remove("\\s+(cbd|city|central|centre|east|west|north|south|act|nsw|vic|qld|wa|sa|tas|nt)$")

  # Sydney CBD-area cluster: guides disagree on which suburb to assign
  # (Crown Sydney is in Barangaroo; Bennelong is at Bennelong Point;
  # Quay is in The Rocks). Treat them all as the same suburb so name
  # matches actually merge.
  cbd_cluster <- c(
    "sydney", "barangaroo", "the rocks", "rocks", "walsh bay",
    "millers point", "dawes point", "circular quay", "bennelong point",
    "east sydney"
  )
  restaurants$suburb_norm <- ifelse(
    restaurants$suburb_norm %in% cbd_cluster,
    "sydney",
    restaurants$suburb_norm
  )

  # Mosman LGA cluster: Balmoral, Cremorne etc. are technically distinct
  # localities but share the Mosman postcode and guides routinely use
  # them interchangeably (e.g. Bathers' Pavilion at 4 The Esplanade is
  # listed under Mosman by AGFG and Balmoral by Time Out).
  mosman_cluster <- c(
    "mosman", "balmoral", "cremorne", "cremorne point",
    "spit junction", "beauty point", "clifton gardens"
  )
  restaurants$suburb_norm <- ifelse(
    restaurants$suburb_norm %in% mosman_cluster,
    "mosman",
    restaurants$suburb_norm
  )

  # Bondi area cluster: guides disagree on Bondi vs Bondi Beach vs
  # North Bondi for venues like Sean's (which is at North Bondi but
  # listed as Bondi by Broadsheet, Bondi Beach by Time Out).
  bondi_cluster <- c("bondi", "bondi beach", "north bondi", "tamarama")
  restaurants$suburb_norm <- ifelse(
    restaurants$suburb_norm %in% bondi_cluster,
    "bondi",
    restaurants$suburb_norm
  )

  # Inner-west cluster: Newtown / Erskineville / Camperdown / Enmore /
  # Stanmore are tightly packed and a single venue (e.g. South End at
  # 644 King St) sits on a postal boundary, so different guides assign
  # different suburbs.
  innerwest_cluster <- c(
    "newtown", "erskineville", "camperdown", "enmore", "stanmore"
  )
  restaurants$suburb_norm <- ifelse(
    restaurants$suburb_norm %in% innerwest_cluster,
    "newtown",
    restaurants$suburb_norm
  )

  restaurants$suburb_norm[is.na(restaurants$suburb) |
                          restaurants$suburb_norm == ""] <- NA_character_

  # Pass 1 - exact normalized name + suburb (treat NA suburb as wildcard)
  groups <- list()
  assigned <- rep(FALSE, nrow(restaurants))

  for (i in seq_len(nrow(restaurants))) {
    if (assigned[i]) next

    name_i <- restaurants$name_norm[i]
    suburb_i <- restaurants$suburb_norm[i]

    # Find all matches: names must be identical, suburbs must match or be NA
    name_match <- restaurants$name_norm == name_i
    suburb_match <- is.na(restaurants$suburb_norm) | is.na(suburb_i) |
      restaurants$suburb_norm == suburb_i
    matches <- which(name_match & suburb_match & !assigned)

    assigned[matches] <- TRUE
    groups <- c(groups, list(matches))
  }

  # Pass 2 - prefix matching for short-name variants like
  # "Ester" / "Ester Restaurant & Bar" or "Bessie's" / "Bessie's and Alma's".
  # When one group's normalized name is a strict word-boundary prefix of
  # another's, with compatible suburbs, fold the shorter into the longer.
  # 4-char minimum prevents overly-greedy merges on tokens like "bar"
  # while still catching short brand names like "LuMi" or "Maiz".
  groups <- gfg_dedup_prefix_pass(groups, restaurants)

  # Pass 3 - address-based matching. Same physical address = same venue,
  # regardless of how the name or suburb is written. Catches cases like
  # Askal (167 Exhibition St, listed as Melbourne by Broadsheet but East
  # Melbourne by AGFG) or O.My (one row says "1/70 Princes Hwy", another
  # "70 Princes Hwy", another "70 Princes Hwy (cnr of Souter St)").
  groups <- gfg_dedup_address_pass(groups, restaurants)

  # Merge each group
  merged <- purrr::map(groups, function(idx) {
    if (length(idx) == 1) {
      row <- restaurants[idx, ]
      row$n_sources <- 1L
      return(row)
    }

    subset <- restaurants[idx, ]

    # Count non-NA values per row to find the "richest"
    richness <- apply(subset, 1, function(r) sum(!is.na(r)))
    best_idx <- which.max(richness)
    merged_row <- subset[best_idx, ]

    # Fill NA values from other rows
    for (col in names(merged_row)) {
      if (col %in% c("source", "name_norm", "suburb_norm", "n_sources")) next
      val <- merged_row[[col]]
      if (length(val) == 0 || is.na(val)) {
        non_na <- subset[[col]][!is.na(subset[[col]])]
        if (length(non_na) > 0) merged_row[[col]] <- non_na[1]
      }
    }

    # Special handling: combine sources
    merged_row$source <- paste(unique(subset$source), collapse = ", ")
    merged_row$n_sources <- length(unique(subset$source))

    # Special handling: prefer longest description
    descs <- subset$description[!is.na(subset$description)]
    if (length(descs) > 0) {
      merged_row$description <- descs[which.max(nchar(descs))]
    }

    # Special handling: prefer non-NA coordinates
    coords <- subset[!is.na(subset$latitude) & !is.na(subset$longitude), ]
    if (nrow(coords) > 0) {
      merged_row$latitude <- coords$latitude[1]
      merged_row$longitude <- coords$longitude[1]
    }

    # Special handling: integer median price across sources, normalized
    # to a 1-4 scale (AU sources sometimes use 1-5; cap at 4 so all
    # cities share the same filter axis). Two-of-three agreement wins;
    # ties round half-to-even (R's default).
    if ("price_range" %in% names(subset)) {
      prices <- subset$price_range[!is.na(subset$price_range)]
      prices <- prices[prices >= 1L]
      prices <- pmin(prices, 4L)
      if (length(prices) > 0) {
        merged_row$price_range <- as.integer(round(stats::median(prices)))
      }
    }

    merged_row
  }) |>
    dplyr::bind_rows()

  # Clean up working columns
  merged$name_norm <- NULL
  merged$suburb_norm <- NULL

  n_removed <- nrow(restaurants) - nrow(merged)
  n_multi <- sum(merged$n_sources > 1)
  cli::cli_alert_success(
    "Deduplicated: {nrow(restaurants)} \u2192 {nrow(merged)} venues ({n_removed} duplicates merged, {n_multi} venue{?s} in multiple guides)"
  )

  merged
}

#' Prefix-match dedup pass
#'
#' Operates on the list of groups produced by Pass 1. Picks a representative
#' (name_norm, suburb_norm) for each group, then folds any group whose name
#' is a strict word-boundary prefix of another's (with compatible suburbs)
#' into the longer-named group. Handles cases like "Ester" /
#' "Ester Restaurant & Bar" where guides shorten venue names.
#'
#' @param groups List of integer vectors (row indices into `restaurants`).
#' @param restaurants Tibble with `name_norm` and `suburb_norm` columns.
#' @noRd
gfg_dedup_prefix_pass <- function(groups, restaurants) {
  n <- length(groups)
  if (n < 2) return(groups)

  # Representative name/suburb per group (first row's values)
  group_name <- vapply(groups, function(idx) restaurants$name_norm[idx[1]], character(1))
  group_suburb <- vapply(groups, function(idx) {
    s <- restaurants$suburb_norm[idx]
    s <- s[!is.na(s)]
    if (length(s) > 0) s[1] else NA_character_
  }, character(1))

  # Union-find via parent vector. Process shortest names first so that a
  # short candidate gets a chance to attach to a longer one before that
  # longer one is itself folded.
  parent <- seq_len(n)
  ord <- order(nchar(group_name))

  # Path-compressed find for union-find ops below
  find_root <- function(x) {
    while (parent[x] != x) x <- parent[x]
    x
  }

  for (i in ord) {
    if (parent[i] != i) next
    name_i <- group_name[i]
    if (is.na(name_i) || nchar(name_i) < 4) next
    suburb_i <- group_suburb[i]
    prefix_with_space <- paste0(name_i, " ")

    # Collect ALL longer-named groups that share the prefix and a
    # compatible suburb - siblings like "LuMi Dining" / "LuMi Bar &
    # Dining" should both fold in via "LuMi" as the connector.
    matches <- integer(0)
    for (j in ord) {
      if (j == i) next
      name_j <- group_name[j]
      if (is.na(name_j) || nchar(name_j) <= nchar(name_i)) next
      if (!startsWith(name_j, prefix_with_space)) next
      suburb_j <- group_suburb[j]
      if (!is.na(suburb_i) && !is.na(suburb_j) && suburb_i != suburb_j) next
      matches <- c(matches, j)
    }

    if (length(matches) == 0) next

    # Union i and all matches into a single group via the first match's root.
    target_root <- find_root(matches[1])
    parent[find_root(i)] <- target_root
    for (k in matches[-1]) {
      kr <- find_root(k)
      if (kr != target_root) parent[kr] <- target_root
    }
  }

  if (all(parent == seq_len(n))) return(groups)

  # Resolve to ultimate roots
  resolve_root <- function(x) {
    while (parent[x] != x) x <- parent[x]
    x
  }
  roots <- vapply(seq_len(n), resolve_root, integer(1))

  # Coalesce groups by root
  unique_roots <- unique(roots)
  lapply(unique_roots, function(r) unlist(groups[which(roots == r)]))
}

#' Address-based dedup pass
#'
#' For each group, derive a canonical street address (number + street name +
#' street type, with units / postcodes / parentheticals stripped). Any groups
#' sharing the same canonical address get merged via union-find. Robust to
#' format variations like "Shop 5/850, Heidelberg-Kinglake Rd, Hurstbridge"
#' vs "850 Heidelberg-Kinglake Rd, Hurstbridge" vs "1/70 Princes Hwy" vs
#' "70 Princes Hwy".
#'
#' @noRd
gfg_dedup_address_pass <- function(groups, restaurants) {
  n <- length(groups)
  if (n < 2) return(groups)

  # Pick the first non-NA address per group (sources usually agree on
  # building-level info even when they disagree on suburb)
  group_addr <- vapply(groups, function(idx) {
    addrs <- restaurants$address[idx]
    addrs <- addrs[!is.na(addrs) & nchar(addrs) > 0]
    if (length(addrs) == 0) return(NA_character_)
    canonical_address(addrs[1])
  }, character(1))

  parent <- seq_len(n)
  find_root <- function(x) {
    while (parent[x] != x) x <- parent[x]
    x
  }

  # Union all groups sharing the same canonical address
  for (a in unique(group_addr[!is.na(group_addr)])) {
    matching <- which(group_addr == a)
    if (length(matching) <= 1) next
    target <- find_root(matching[1])
    for (k in matching[-1]) {
      kr <- find_root(k)
      if (kr != target) parent[kr] <- target
    }
  }

  if (all(parent == seq_len(n))) return(groups)
  roots <- vapply(seq_len(n), find_root, integer(1))
  unique_roots <- unique(roots)
  lapply(unique_roots, function(r) unlist(groups[which(roots == r)]))
}

#' Reduce a freeform address string to "<number> <street name> <type>"
#'
#' Handles unit prefixes (Shop 5/850, Suite 12/, 1/70), state codes,
#' postcodes, parenthetical notes, and Latin-accented characters. Returns
#' `NA` if no street pattern can be found.
#'
#' @noRd
canonical_address <- function(addr) {
  if (is.na(addr) || addr == "" || nchar(addr) < 5) return(NA_character_)

  addr_norm <- addr |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower() |>
    # Strip parenthetical notes like "(cnr of Souter St)"
    stringr::str_remove_all("\\([^)]*\\)") |>
    # Collapse "1/70" / "5/850" -> just the building number
    stringr::str_replace_all("\\b\\d+/(\\d+)\\b", "\\1") |>
    # Treat commas as separators so "Shop 5/850, Heidelberg-Kinglake Rd"
    # collapses to a single space-separated string the pattern can find
    stringr::str_replace_all(",", " ") |>
    stringr::str_squish()

  street_types <- paste(
    c("street", "st", "road", "rd", "avenue", "ave", "highway", "hwy",
      "drive", "dr", "place", "pl", "parade", "pde", "crescent", "cres",
      "lane", "ln", "terrace", "tce", "boulevard", "blvd", "circuit",
      "cct", "court", "ct", "way"),
    collapse = "|"
  )
  # Number + street name + street type. The `\\d+\\s+[a-z]` start excludes
  # leading unit numbers like "3 850 ..." (since "3 8..." has a digit after
  # the space, not a letter).
  pattern <- paste0(
    "\\b(\\d+)\\s+([a-z][a-z0-9\\s\\-']*?)\\s+(", street_types, ")\\b"
  )

  match <- stringr::str_match(addr_norm, pattern)
  if (is.na(match[1, 1])) return(NA_character_)

  match[1, 1] |>
    stringr::str_replace_all("\\bstreet\\b", "st") |>
    stringr::str_replace_all("\\broad\\b", "rd") |>
    stringr::str_replace_all("\\bavenue\\b", "ave") |>
    stringr::str_replace_all("\\bhighway\\b", "hwy") |>
    stringr::str_replace_all("\\bparade\\b", "pde") |>
    stringr::str_replace_all("\\bcrescent\\b", "cres") |>
    stringr::str_replace_all("\\bboulevard\\b", "blvd") |>
    stringr::str_replace_all("\\bterrace\\b", "tce") |>
    stringr::str_replace_all("\\blane\\b", "ln") |>
    stringr::str_replace_all("\\bplace\\b", "pl") |>
    stringr::str_replace_all("\\bdrive\\b", "dr") |>
    stringr::str_replace_all("\\bcircuit\\b", "cct") |>
    stringr::str_replace_all("\\bcourt\\b", "ct") |>
    stringr::str_squish()
}
