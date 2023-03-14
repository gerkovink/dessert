#' \pkg{Dessert} recipes
#'
#' @description
#' Run a \pkg{Dessert} recipe on a data set or model to create a reproducible
#' standard publication archive.
#'
#' @param object The input data set or model which forms the base of the dessert.
#' @param from A string specifying the package from which the input data set or model originates.
#' @param recipe A string specifying a \pkg{Dessert} recipe associated with the input data set or model. See the recipe book in details for available recipes. The default behavior is to match all desserts if no recipe is specified.
#' @param output_format A string specifying the output format of the \code{quarto} document. Available ouput formats are `"html"`, `"pdf"`, and `"docx"`. By default, all output formats are provided.
#' @param output_dir A string specifying the output directory for the output files.
#'
#' @details The \pkg{Dessert} recipe book:
#'
#' | **class**   | **from**   | **recipe(s)**                | **details**      |
#' |-------------|------------|------------------------------|------------------|
#' | `lm`        | `stats`    | `"regression"`, `"ancova"`   | [dessert.lm()]   |
#'
#' @md
#'
#' @return A reproducible standard publication archive with the following folder structure:
#' \preformatted{
#' `r paste0("| - dessert_", format(Sys.time(), "%Y-%m-%d_%H_%M_%S"), "/")`
#'   | - class_package/
#'     | - recipe.RData
#'     | - recipe.qmd
#'     | - recipe.html
#'     | - recipe.pdf
#'     | - recipe.docx
#'     | - recipe/
#'       | - figure_1.png
#'       | - figure_2.png
#'       | - ...
#'   | - ...
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fit <- lm(dist ~ speed, data = cars)
#' dessert(fit, recipe = "regression")
#' }
dessert <- function(
  object,
  from          = NULL,
  recipe        = NULL,
  output_dir    = NULL,
  output_format = "all") {

  my_test_function(1)
  my_test_function_2(2)
  # check if the user is not cooking up some evil dish
  if (!(output_format %in% c("all", "html", "pdf", "docx"))) {
    stop(
      paste0(
        "Dessert with output format: \"", output_format, "\" cannot be served. Valid ouput formats are \"html\", \"pdf\", and \"docx\"."
      )
    )
  }

  # think of a plate to serve the dessert on
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

  # check if a dessert can be served on this hypothetical plate
  if (file.access(output_dir, 2) != 0) {
    stop(paste0("Dessert cannot be served on: ", output_dir, ". Please provide a valid output directory."))
  }

  # load in the recipes book
  cookbook_dir <- paste(
    system.file(package = "dessert"), "cookbook", "recipes.csv", sep = '/'
  )
  if (!file.exists(cookbook_dir)) {
    stop("Ohew no, the Dessert recipe book is lost! Ensure Dessert is installed.")
  }
  cookbook <- read.csv(cookbook_dir)

  # future code if there is a lot of overlap between classes some can be hidden
  cookbook <- cookbook[, -4]

  # tear out all unwanted recipes from the recipes book
  if (!is.null(from)) {
    cookbook <- cookbook[cookbook$from == from, ]
  }
  if (!is.null(recipe)) {
    cookbook <- cookbook[cookbook$recipe == recipe,]
  }
  cookbook <- cookbook[cookbook$class == class(object),]

  # case a: there are no recipes available.
  if (nrow(cookbook) == 0L) {
    stop("No recipe available for the provided dataset or object.")
  }

  # now we are cooking, create an actual plate to serve on
  output_dir <- paste(
    output_dir, paste0("dessert_", format(Sys.time(), "%Y-%m-%d_%H_%M_%S")),
    sep = '/'
  )
  if (!file.exists(output_dir)) {
    dir.create(output_dir)
    print(output_dir)
  } else {
    stop(paste0("Dessert cannot be served on: ", output_dir, ". Please provide a unique output directory."))
  }

  # case b: there is a unique recipe
  if (nrow(cookbook) == 1L) {
    print("case b: unique recipe")

    do.call(
      paste("dessert", cookbook$class, sep = "."),
      args = list(
        object        = object,
        from          = cookbook$from,
        recipe        = cookbook$recipe,
        output_format = output_format,
        output_dir    = output_dir
      )
    )

    return(TRUE)
  }

  # case c: no unique recipe
  print("case c: no unique recipe")

  # ask the user which option they prefer
  cat("There are multiple recipes available.\n\n")
  cookbook <- rbind(
    data.frame(class = "all", from = "all", recipe = "all"),
    cookbook
  )
  rownames(cookbook) <- 1:nrow(cookbook)
  print(cookbook)
  cat("\nWhich one(s) should we prepare?\n")

  prompt <- paste0("Select (", paste(rownames(cookbook), collapse = "/"), "): ")
  input <- as.integer(readline(prompt = prompt))
  if (!(is.integer(input) & input %in% 1:nrow(cookbook))) {
    stop("That option is not in our recipe book.")
  }
  if (input == 1L) {
    cookbook <- cookbook[-1,]
  } else {
    cookbook <- cookbook[input,]
  }

  for (index in 1:nrow(cookbook)) {
    do.call(
      paste("dessert", cookbook[index,]$class, sep = "."),
      args = list(
        object        = object,
        from          = cookbook[index,]$from,
        recipe        = cookbook[index,]$recipe,
        output_format = output_format,
        output_dir    = output_dir
      )
    )
  }

  return(TRUE)
}

