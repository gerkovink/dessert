#' \pkg{Dessert} recipes
#'
#' @md
#'
#' @description Run a \pkg{Dessert} recipe on a data set or model to create a
#' reproducible standard publication archive.
#'
#' @param input The input data set or model which forms the base of the dessert.
#' @param cls A string that specifying a class from the input data or model.
#' @param from A string specifying the package from which the input data set or model originates.
#' @param recipe A string specifying a \pkg{Dessert} recipe associated with the input data set or model. See the recipe book in details for available recipes. The default behavior is to match all desserts if no recipe is specified.
#' @param output_dir A string specifying the output directory for the output files.
#' @param output_format A string specifying the output format of the \code{quarto} document. Available ouput formats are `"html"`, `"pdf"`, and `"docx"`. By default, all output formats are provided.
#'
#' @details The \pkg{Dessert} recipe book:
#'
#' | **class**   | **from**   | **recipe(s)**                | **details**      |
#' |-------------|------------|------------------------------|------------------|
#' | `lm`        | `stats`    | `"regression"`, `"ancova"`   | [dessert.lm()]   |
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
    ...) {

  dessert <- Dessert$new(
    input         = input,
    cls           = cls,
    from          = from,
    recipe        = recipe,
    output_dir    = output_dir,
    output_format = output_format,
    ...
  )
  dessert$render()

  return(dessert)
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

      self$input <- input

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
        stop("There is no dessert available.")
      }
      if (nrow(options) > 1L) {
        mat <- as.matrix(options)
        mat <- cbind(1:nrow(mat), mat)
        mat <- rbind(colnames(mat), mat)
        mat <- apply(mat, 2, format)

        cat("There are multiple desserts available.\n\n")
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
          stop("Kitchen is closed.")
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

      private$.output_dir <- paste(
        private$.output_dir,
        paste0("dessert_", format(Sys.time(), "%Y-%m-%d_%H_%M_%S")),
        sep = '/'
      )

      do.call(
        paste("dessert", private$.cls, sep = "."),
        args = list(self = self)
      )
    },
    render = function(...) {
      print("method render")

      dir_pkg <- system.file(package = "dessert")

      dir_qmd <- paste(
        dir_pkg, "qmd", self$cls, paste(self$recipe, "qmd", sep = "."),
        sep = "/"
      )
      if (file.exists(dir_qmd)) {
        cat("\033[31mdessert: qmd located\n\n\033[0m")
      } else {
        stop("Quarto markdown not found")
      }

      dir_files <- grep("\\.png)", readLines(dir_qmd), value = TRUE)
      dir_files <- gsub(".*/(.*?)\\).*", "\\1", dir_files)
      if (length(dir_files) > 0) {
        dir_files <- paste(
          system.file(package = "dessert"), "help", "figures", dir_files,
          sep = "/"
        )
        files_exist <- file.exists(dir_files)
        if (all(files_exist)) {
          cat("\033[31mdessert: figures located\n\n\033[0m")
        } else {
          stop("Following figures not found: ", dir_files[!files_exist])
        }
      }

      if (file.exists(self$output_dir)) {
        stop(
          "Dessert cannot be served on: ", self$output_dir, ".",
          "Please provide a unique output directory."
        )
      } else {
        dir.create(self$output_dir)
        if (length(dir_files) > 0) {
          output_files <- paste(self$output_dir, self$recipe, sep = "/")
          dir.create(output_files)
        }
        cat("\033[31mdessert: output folder created\n\n\033[0m")
      }

      if (file.copy(from = dir_qmd, to = self$output_dir, overwrite = FALSE)) {
        cat("\033[31mdessert: qmd copies\n\n\033[0m")
      } else {
        stop("Quarto markdown file not copied")
      }
      if (length(dir_files) > 0) {
        if (
          all(
            file.copy(
              from = dir_files,
              to = output_files,
              overwrite = FALSE
            )
          )
        ) {
          cat("\033[31mdessert: figures copied\n\n\033[0m")
        } else {
          stop("Figures not copied")
        }
      }

      input <- self$input
      save(
        input = input,
        file = paste(
          self$output_dir,
          paste(self$recipe, "rdata", sep = "."),
          sep = "/"
        )
      )
      cat("\033[31mdessert: input object saved\n\n\033[0m")

      quarto::quarto_render(
        input         = paste(
          self$output_dir,
          paste(self$recipe, "qmd", sep = "."),
          sep = "/"
        ),
        output_format = self$output_format,
        quiet         = FALSE
      )
    },
    change_dessert = function(cls, from, recipe) {
      print("method change_dessert")

      if (nrow(merge(data.frame(class = cls, from, recipe), recipes)) > 0) {
        private$.cls <- cls
        private$.from <- from
        private$.recipe <- recipe
      } else {
        if (!cls %in% recipes$class) {
          stop("Invalid cls field.")
        }
        if (!from %in% recipes$from) {
          stop("Invalid from field.")
        }
        if (!recipe %in% recipes$recipe) {
          stop("Invalid recipe field.")
        }
      }
    },
    print = function(...) {
      cat("my print")
    }
  ),
  private = list(
    .cls = NA,
    .from = NA,
    .recipe = NA,
    .output_dir = NA,
    .output_format = NA
  ),
  active = list(
    cls = function(value = NULL) {
      if (is.null(value)) {
        private$.cls
      } else {
        stop("Field cls is read-only. Use the method change_dessert().")
      }
    },
    from = function(value = NULL) {
      if (is.null(value)) {
        private$.from
      } else {
        stop("Field from is read-only. Use the method change_dessert().")
      }
    },
    recipe = function(value = NULL) {
      if (is.null(value)) {
        private$.recipe
      } else {
        stop("Field recipe is read-only. Use the method change_dessert().")
      }
    },
    output_dir = function(value = NULL) {
      if (is.null(value)) {
        private$.output_dir
      } else {
        stop("Field output_dir is read-only.")
      }
    }
  ),
  cloneable = FALSE
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
