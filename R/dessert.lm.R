#' \pkg{Dessert} recipes for class `lm`
#'
#' @description
#' \pkg{Dessert} recipe(s) for objects with class `lm` from the `stats` package.
#'
#' @details
#'
#' * ancova
#' * regression
#'
#' @md
#' @export
dessert.lm <- function(dessert, ...) {
  print("dessert.lm")

  dessert@parameters <- list(a = 123)

  return(dessert)
}
