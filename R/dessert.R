#' \pkg{Dessert} recipes
#'
#' @description Run a \pkg{Dessert} recipe on a data set or model to create a
#' reproducible standard publication archive.
#'
#' @param input The input data set or model which forms the base of the dessert.
#' @param cls A string that specifying a class from the input data or model.
#' @param from A string specifying the package from which the input data set or model originates.
#' @param recipe A string specifying a \pkg{Dessert} recipe associated with the input data set or model. See the recipe book in details for available recipes. The default behavior is to match all desserts if no recipe is specified.
#' @param output_format A string specifying the output format of the \code{quarto} document. Available ouput formats are `"html"`, `"pdf"`, and `"docx"`. By default, all output formats are provided.
#' @param output_dir A string specifying the output directory for the output files.
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
#'   | - recipe.RData
#'   | - recipe.qmd
#'   | - recipe.html
#'   | - recipe.pdf
#'   | - recipe.docx
#'   | - recipe/
#'     | - figure_1.png
#'     | - ...
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
    input,
    cls           = NULL,
    from          = NULL,
    recipe        = NULL,
    output_dir    = NULL,
    output_format = "all",
    quiet         = TRUE) {

  output <- Dessert$new(
    input         = input,
    cls           = cls,
    from          = from,
    recipe        = recipe,
    output_dir    = output_dir,
    output_format = output_format
  )
  output$render()

  return(output)
}


Dessert <- R6::R6Class(
  classname = "Dessert",
  public = list(
    input = NA,
    initialize = function(
        input,
        cls = NULL,
        from = NULL,
        recipe = NULL,
        output_dir = NULL,
        output_format = "all",
        ...) {
      print("method initialize")

      if (is.null(cls)) {
        cls <- class(input)
      } else {
        if (!is.character(cls) | !length(cls) == 1) {
          stop("Argument cls should be a string.")
        }
        if (!cls %in% class(input)) {
          stop("Argument cls is not in the input dataset or object classes.")
        }
      }
      options <- recipes[recipes$class %in% cls,]

      if (!is.null(from)) {
        if (!is.character(from) | !length(from) == 1) {
          stop("Argument from should be a string.")
        }
        options <- options[options$from == from,]
      }

      if (!is.null(recipe)) {
        if (!is.character(recipe) | !length(recipe) == 1) {
          stop("Argument recipe should be a string.")
        }
        options <- options[options$recipe == recipe,]
      }

      if (nrow(options) == 0L) {
        stop("There is no recipe available.")
      }
      if (nrow(options) > 1L) {
        mat <- as.matrix(options)
        mat <- cbind(1:nrow(mat), mat)
        mat <- rbind(colnames(mat), mat)
        mat <- apply(mat, 2, format)

        cat("There are multiple recipes available.\n\n")
        for (i in 1:nrow(mat)) {
          cat(mat[i,1], mat[i,2], mat[i,3], mat[i,4], "\n", sep = "   ")
        }
        cat("\nWhich one should we prepare?\n")
        prompt <- paste(c(rownames(options), "None"), collapse = "/")
        input <- readline(prompt = paste0("Select (", prompt, "): "))

        if (!input %in% c(1:nrow(options), "None")) {
          stop("Input is not valid.")
        }
        if (input == "None") {
          stop("Kitchen is closed")
        }
        options <- options[input,]
      }

      private$.cls <- options$class
      private$.from <- options$from
      private$.recipe <- options$recipe

      if (is.null(output_dir)) {
        if (!rstudioapi::isAvailable()) {
          private$.output_dir <- getwd()
        } else {
          output_dir <- rstudioapi::getSourceEditorContext()$path
          if (is.null(output_dir)) {
            private$.output_dir <- getwd()
          } else {
            private$.output_dir <- dirname(output_dir)
          }
        }
      }
    },
    render = function() {
      print("method render")
    }
  ),
  private = list(
    .cls = NA,
    .from = NA,
    .recipe = NA,
    .output_dir = NA
  )
)


recipes <- data.frame(
  matrix(
    c(
      "lm", "stats", "regression",
      "lm", "stats", "ancova",
      "data.frame", "base", "missing"
    ),
    ncol = 3,
    byrow = TRUE,
    dimnames = list(NULL, c("class", "from", "recipe"))
  )
)
