#' \pkg{Dessert} recipes
#'
#' @description
#' Run a dessert recipe on a data set or model to create a reproducible standard
#' publication archive.
#'
#' @param object The input data set or model which forms the base of the dessert.
#' @param output_format The output format parsed to \code{quarto} document.
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
  recipe        = NULL,
  output_dir    = NULL,
  output_format = NULL) {

  # obtain the plate to serve the dessert on
  if (is.null(output_dir)) {
    if (!rstudioapi::isAvailable()) {
      output_dir <- getwd()
    } else {
      output_dir <- rstudioapi::getSourceEditorContext()$path
      if (is.null(output_dir)) {
        output_dir <- getwd()
      } else {
        output_dir <- dirname(output_dir)
      }
    }
  }

  # check if the output directory can be used to store results
  if (file.access(output_dir, 2) != 0) {
    stop(paste0("Dessert cannot be served on: ", output_dir, ". Please provide a valid output directory."))
  }

  # load in the recipes book
  cookbook_dir <- paste(
    system.file(package = "dessert"), "cookbook", "recipes.csv", sep = "/"
  )
  if (!file.exists(cookbook_dir)) {
    stop("Ohew no, the Dessert recipe book is lost! Ensure Dessert is installed.")
  }
  cookbook <- read.csv(cookbook_dir)

  # tear out all unwanted recipes from the cookbook
  if (!is.null(recipe)) {
    cookbook <- cookbook[cookbook$recipes == recipe,]
  }
  cookbook <- cookbook[cookbook$class == class(object),]

  # case a: no recipes
  if (nrow(cookbook) == 0L) {
    stop("No recipe available for the provided dataset or object.")
  }

  # create an output folder
  output_dir <- paste(
    output_dir, paste0("dessert_", format(Sys.time(), "%Y-%m-%d_%H_%M_%S")),
    sep = "/"
  )

  if (!file.exists(output_dir)) {
    #dir.create(output_dir)
    print(output_dir)
  } else {
    stop(paste0("Dessert cannot be served on: ", output_dir, ". Please provide a valid output directory."))
  }

  # case b: unique recipe
  if (nrow(cookbook) == 1L) {
    print("case b: unique recipe")

    do.call(
      paste("dessert", cookbook$class, sep = "."),
      args = list(
        object        = object,
        recipe        = cookbook$recipe,
        output_format = output_format,
        output_dir    = output_dir
      )
    )

    return(TRUE)
  }

  # case c: no unique recipe
  print("case c: no unique recipe")
  # 1 locatie
  # images

  #cat(
  #  "Recipes for the provided dataset or object:", "", "0) all",
  #  base::paste0(seq_along(cls), ") ", cls), "",
  #  sep = "\n"
  #)

  #prompt <- sprintf(
  #  "Select (%s): ",
  #  paste(c("0", seq_along(cls)), collapse = "/")
  #)
  #input <- as.integer(readline(prompt = prompt))

  #if (input != 0L) cls <- cls[input]

  # dessert_lm

  #purrr::walk(
  #  .x = cls,
  #  .f = ~ do.call(
  #    cls,
  #    args = list(
  #      object        = object,
  #      output_format = output_format,
  #      output_dir    = output_dir
  #    )
  #  )
  #)
}

