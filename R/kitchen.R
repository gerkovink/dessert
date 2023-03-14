#' Title of the function
#'
#' A short description of what the function does.
#' @noRd
dessert_copy <- function(from, recipe, output_dir) {
  # obtain the class name
  cls <- gsub("^[^.]+\\.", "", sys.call(which = -1)[[1]])
  cls <- gsub("\\.", "_", cls)

  # create the specific class package output folder
  output_dir <- paste(output_dir, paste(cls, from, sep = '_'), sep = '/')

  if (!file.exists(output_dir)) {
    dir.create(output_dir)
  }

  # copy the quarto markdown to the output folder
  qmd_dir <- paste(
    system.file(package = "dessert"), "qmd",
    paste(cls, from, sep = '_'),
    paste(recipe, "qmd", sep = '.'),
    sep = '/'
  )

  file.copy(from = qmd_dir, to = output_dir, overwrite = FALSE)

  # copy the additional files to the output folder
  img_dir <- list.files(
    path = paste(
      system.file(package = "dessert"), "qmd",
      paste(cls, from, sep = '_'),
      recipe,
      sep = '/'
    ),
    full.names = TRUE
  )

  if (length(img_dir) != 0) {
    dir.create(paste(output_dir, recipe, sep = '/'))

    file.copy(
      from      = img_dir,
      to        = paste(output_dir, recipe, sep = '/'),
      overwrite = FALSE,
      recursive = TRUE,
      copy.mode = FALSE
    )
  }
}


#' Title of the function
#'
#' A short description of what the function does.
#' @noRd
dessert_save <- function(object, from, recipe, output_dir, ...) {
  # obtain the class name
  cls <- gsub("^[^.]+\\.", "", sys.call(which = -1)[[1]])
  cls <- gsub("\\.", "_", cls)

  # set the output directory to the output folder
  output_dir <- paste(
    output_dir,
    paste(cls, from, sep = "_"),
    paste(recipe, "rdata", sep = "."), sep = "/"
  )

  # store the input object and potential parameters
  parameters <- list(...)
  if (length(parameters) == 0) {
    save(object, file = output_dir)
  } else {
    save(object, parameters, file = output_dir)
  }
}


#' Title of the function
#'
#' A short description of what the function does.
#' @noRd
dessert_cook <- function(from, recipe, output_dir, output_format, quiet) {
  # obtain the class name
  cls <- gsub("^[^.]+\\.", "", sys.call(which = -1)[[1]])
  cls <- gsub("\\.", "_", cls)

  qmd_dir <- paste(
    output_dir,
    paste(cls, from, sep = "_"),
    paste(recipe, "qmd", sep = '.'),
    sep = '/'
  )

  if (output_format == "all") {
    output_format <- c("html", "pdf", "docx")
  }

  for (format in output_format) {
    tryCatch({
      # Code to be executed
      quarto::quarto_render(
        input = qmd_dir,
        output_format = output_format,
        quiet = quiet
      )

      # Additional code to be executed if no error occurs
      cat("\033[32m", "No errors occured in: ", format, "\033[0m\n", sep = "")
    },
    error = function(e) {
      print(format)
      print(e)
      cat("\033[31mThis text is printed in red!\033[0m\n")
    })
  }
}
