#' \pkg{Dessert} recipes
#'
#' @description
#' Run a dessert recipe on a data set or model.
#'
#' @param object The input data set or model which forms the base of the dessert.
#' @param output_format The output format parsed to \code{quarto}.
#' @param output_dir The output directory for the rendered output files.
#'
#' @return A reproducible standard publication archive.
#' @export
#'
#' @examples
#' \dontrun{
#' fit <- lm(dist ~ speed, data = cars)
#' dessert(fit)
#' }
dessert <- function(
  object,
  output_format = NULL,
  output_dir    = NULL) {

  # check available recipes for object
  cls_dessert <- base::ls("package:dessert", pattern = "^dessert.")
  cls_dessert <- c("dessert.tbl", "dessert.data.frame", "dessert.lm")
  cls <- base::class(object)
  cls <- base::paste("dessert", cls, sep = ".")
  cls <- base::intersect(cls, cls_dessert)

  # case a: no recipes
  if (length(cls) == 0) stop("No recipe available for the provided dataset or object.")

  # case b: unique recipe
  if (length(cls) == 1) {
    do.call(
      cls,
      args = list(
        object        = object,
        output_format = output_format,
        output_dir    = output_dir
      )
    )
    return(TRUE)
  }

  # case c: multiple recipe
  cat(
    "Recipes for the provided dataset or object:",
    "",
    "0) all",
    base::paste0(seq_along(cls), ") ", cls),
    "",
    sep = "\n"
  )

  prompt <- sprintf(
    "Select (%s): ",
    paste(c("0", seq_along(cls)), collapse = "/")
  )
  input <- as.integer(readline(prompt = prompt))

  if (input != 0L) cls <- cls[input]

  purrr::walk(
    .x = cls,
    .f = ~
    do.call(
      cls,
      args = list(
        object        = object,
        output_format = output_format,
        output_dir    = output_dir
      )
    )
  )
}

