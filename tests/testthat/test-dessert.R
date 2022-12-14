# Create a linear model
fit <- lm(dist ~ speed, data = cars)
testthat::expect_true(class(fit) == "lm")
# Test if runs
testthat::expect_no_error(dessert(fit, output_format = "html_document"))
# Test if storage location is communicated
testthat::expect_output(dessert(fit, output_format = "html_document"))
# Test if files are produced
testthat::expect_true(file.exists("dessert_envir.RData"))
testthat::expect_true(file.exists("lm.Rmd"))
testthat::expect_true(file.exists("lm.html"))
# Remove produced files from test environment
unlink(c("dessert_envir.RData",
         "lm.Rmd",
         "lm.html"))

