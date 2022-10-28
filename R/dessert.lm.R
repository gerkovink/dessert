#' Run a dessert recipe on an analysis or data set
#'
#' @param data The input data set or fit object to call dessert on.
#' @param output_format The R markdown output format parsed to `rmarkdown`.
#' @param output_dir The output directory for the rendered output files.
#'
#' @return A reproducible standard publication archive
#' @export
#'
#' @examples
#'
#' # Call a lm dessert on a fitted linear model
#'
#' fit <- lm(dist ~ speed, data = cars)
#' dessert.lm(fit, output_format = "html_document")
#'
#' # Remove the generated files in R-CMD-Check
#' # Don't run as an example in real life
#' unlink(c("dessert_envir.RData", "lm.Rmd", "lm.html"))

dessert.lm <- function(data,
                       output_format = NULL,
                       output_dir = NULL) {
  # rmd location
  rmdloc <-
    paste(.libPaths(), "dessert", "rmd", "lm.Rmd", sep = "/")

  # get the directory of the file calling dessert
  if (!rstudioapi::isAvailable()) {
    if (is.null(output_dir)) {
      output_dir <- getwd()
    }
  } else {
    if (is.null(output_dir)) {
      output_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
    }

    # if the file calling is not yet stored, use the work directory
    if (!nzchar(output_dir)) {
      # THE ABOVE CODE YIELDS PROBLEMS IN THE FUNCTION ENVIRONMENT
      output_dir <- getwd()
    }
  }

  if (file.access(output_dir, 2) != 0) {
    cat(
      paste(
        "No write access to the output directory: ",
        output_dir,
        ". Please specify an output directory with sufficient access privileges.",
        sep = ""
      )
    )
  } else {
    # copy the .Rmd file to the output location
    file.copy(
      from = rmdloc,
      to = output_dir,
      overwrite = TRUE,
      recursive = TRUE,
      copy.mode = TRUE
    )

    # default output is html, pdf, word, setting output format to all
    if (is.null(output_format)) {
      rmarkdown::render(
        input = paste(output_dir, "lm.rmd", sep = "/"),
        output_format = "all",
        output_dir = output_dir,
        quiet = TRUE
      )
    }
    # user specified output format
    else {
      rmarkdown::render(
        input = paste(output_dir, "lm.rmd", sep = "/"),
        output_format = output_format,
        output_dir = output_dir,
        quiet = TRUE
      )
    }

    # store the data as RData
    save(data, file = paste(output_dir, "dessert_envir.RData", sep = "/"))

    # print statement
    cat("files have been stored in", output_dir)
  }
}
