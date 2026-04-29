# Rule-based price inference for venues that no source reported a price
# for. Always sets a `price_inferred` boolean so downstream consumers
# (popup labels, filters) can distinguish source-reported from
# rule-filled values. Rules are deterministic — same input always
# produces the same output, no model calls or stochastic guesses.


#' Fill missing `price_range` from venue metadata using transparent rules
#'
#' Designed to run on the deduplicated tibble (one row per venue) so
#' multi-source agreement has already happened. Rules are applied in
#' priority order; the first match wins. Always adds (or refreshes) a
#' `price_inferred` logical column.
#'
#' Rules:
#' \enumerate{
#'   \item Michelin Three Stars or Two Stars -> 4 (premium)
#'   \item Michelin One Star                 -> 3
#'   \item Michelin Bib Gourmand             -> 2 (good value)
#'   \item Description matches "fine dining|tasting menu|degustation|
#'         chef's table|prix fixe"           -> 4
#'   \item Cuisine matches "cafe|deli|bakery|takeaway|sandwich|noodle bar|
#'         juice bar|cake shop|food truck"   -> 1
#'   \item Cuisine matches "pizza|pizzeria|ramen|dumpling|pho|taqueria|
#'         burger|bbq"                       -> 2
#'   \item No match: leave NA (price_inferred FALSE)
#' }
#'
#' Michelin Selected venues without a `priceRange` field deliberately
#' don't get an inferred price — Selected covers a huge spread (cheap
#' Filipino takeaway alongside upmarket bistros) and any single value
#' would be misleading.
#'
#' @param restaurants A post-dedup tibble. Must have a `price_range`
#'   column; `cuisine`, `description`, and `michelin_distinction` are
#'   used opportunistically when present.
#' @return The same tibble with `price_range` filled where rules apply
#'   and a `price_inferred` logical column. Existing non-NA prices and
#'   their `price_inferred = FALSE` flag are preserved.
#' @export
infer_missing_price <- function(restaurants) {
  if (!"price_range" %in% names(restaurants)) {
    cli::cli_warn("No {.field price_range} column; nothing to infer.")
    restaurants$price_inferred <- logical(nrow(restaurants))
    return(restaurants)
  }

  pr <- restaurants$price_range
  if (!is.integer(pr)) pr <- as.integer(pr)
  inferred <- rep(FALSE, nrow(restaurants))
  needs <- is.na(pr)

  cuisine_low <- tolower(restaurants$cuisine %||% rep(NA_character_, nrow(restaurants)))
  desc_low    <- tolower(restaurants$description %||% rep(NA_character_, nrow(restaurants)))
  mich        <- if ("michelin_distinction" %in% names(restaurants)) {
    restaurants$michelin_distinction
  } else {
    rep(NA_character_, nrow(restaurants))
  }

  # Rule application helper - applies `value` to rows where `mask` is
  # TRUE and the venue still needs a price, then marks them inferred.
  apply_rule <- function(mask, value) {
    hits <- needs & !is.na(mask) & mask
    pr[hits] <<- value
    inferred[hits] <<- TRUE
    needs[hits] <<- FALSE
  }

  apply_rule(!is.na(mich) & mich %in% c("Three Stars", "Two Stars"), 4L)
  apply_rule(!is.na(mich) & mich == "One Star",                       3L)
  apply_rule(!is.na(mich) & mich == "Bib Gourmand",                   2L)
  apply_rule(
    grepl(
      "\\b(fine dining|tasting menu|degustation|chef'?s table|prix fixe)\\b",
      desc_low
    ),
    4L
  )
  apply_rule(
    grepl(
      "\\b(cafe|café|deli|bakery|takeaway|sandwich(es)?|noodle bar|juice bar|cake shop|food truck)\\b",
      cuisine_low
    ),
    1L
  )
  apply_rule(
    grepl(
      "\\b(pizza|pizzeria|ramen|dumpling|pho|taqueria|burger|bbq|barbecue)\\b",
      cuisine_low
    ),
    2L
  )

  restaurants$price_range    <- pr
  restaurants$price_inferred <- inferred

  n_filled <- sum(inferred)
  n_still <- sum(is.na(pr))
  cli::cli_alert_info(
    "Inferred price for {n_filled} venue{?s} via rules; {n_still} remain NA."
  )

  restaurants
}
