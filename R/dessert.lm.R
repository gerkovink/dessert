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
dessert.lm <- function(
  object,
  from,
  recipe,
  output_dir,
  output_format = "all",
  quiet         = TRUE) {

  # copy the recipe to the output folder
  dessert_copy(from, recipe, output_dir)

  # copy the recipe to the output folder
  dessert_save(object, from, recipe, output_dir)

  # render the files
  dessert_cook(from, recipe, output_dir, output_format, quiet)

  return(TRUE)
}
