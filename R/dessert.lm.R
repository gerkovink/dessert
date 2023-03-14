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
#'
dessert.lm <- function(
  object,
  from,
  recipe,
  output_dir,
  output_format = "all") {

  # print statement for debugging
  cat(
    "function: dessert.lm \n",
    "package: ", from, "\n",
    "recipe: ", recipe, "\n",
    "output_dir: ", output_dir, "\n",
    "output_format: ", output_format, "\n"
  )

  # class name
  cls <- sub(".*\\.", '', sys.call()[[1]])

  # create new directory for the class results
  output_dir <- paste(output_dir, paste(cls, from, sep = '_'), sep = '/')

  if (!file.exists(output_dir)) {
    dir.create(output_dir)
  }

  # copy the quarto file
  qmd_dir <- paste(
    system.file(package = "dessert"), "qmd",
    paste(cls, from, sep = '_'),
    paste(recipe, "qmd", sep = '.'),
    sep = '/'
  )

  file.copy(
    from      = qmd_dir,
    to        = output_dir,
    overwrite = FALSE,
    recursive = FALSE,
    copy.mode = FALSE
  )


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

  # store the object as a rdata
  save(
    object,
    file = paste(output_dir, paste(recipe, "rdata", sep = '.'), sep = '/')
  )

  # render the quarto files
  quarto::quarto_render(
    input = paste(output_dir, paste(recipe, "qmd", sep = '.'), sep = '/'),
    output_format = output_format
  )

  return(TRUE)
}
