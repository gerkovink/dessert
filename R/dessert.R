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
#' \dontrun{
#' fit <- lm(dist ~ speed, data = cars)
#' dessert(fit)
#' }
dessert <- function(data,
                    output_format = NULL,
                    output_dir = NULL) {

  # determine object class
  cls <- class(data)

  # call the corresponding dessert based on the first class
  do.call(paste("dessert", cls[1], sep = "."),
          args = list(data = data,
                      output_format = output_format,
                      output_dir = output_dir)
          )
}
