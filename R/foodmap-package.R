#' @keywords internal
"_PACKAGE"

# Package-wide imports needed only for namespace registration:
# - `rlang::.data` is referenced inside dplyr verbs (e.g.
#   dplyr::case_when(.data$hats > 1, ...)) so R CMD check stops
#   complaining about an undefined global variable.
# - `NA_Date_` lives in base R but R CMD check's binding analysis
#   doesn't recognise it without a hint.
#' @importFrom rlang .data
#' @importFrom utils globalVariables
NULL

utils::globalVariables(c("NA_Date_"))
