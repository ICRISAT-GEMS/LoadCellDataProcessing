#' getmode 
#'
#' @description function to calculate mode
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}