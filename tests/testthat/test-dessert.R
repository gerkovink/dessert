# Create a linear model
fit <- lm(dist ~ speed, data = cars)
# Test if runs
testthat::expect_no_error(dessert(fit))
# Test if storage location is communicated
testthat::expect_output(dessert(fit))
# Test if files are produced
testthat::expect_true(file.exists("dessert_envir.RData"))
testthat::expect_true(file.exists("lm.Rmd"))
testthat::expect_true(file.exists("lm.docx"))
testthat::expect_true(file.exists("lm.html"))
testthat::expect_true(file.exists("lm.pdf"))
# Remove produced files from test environment
unlink(c("dessert_envir.RData", "lm.Rmd", "lm.docx", "lm.html", "lm.pdf"))
