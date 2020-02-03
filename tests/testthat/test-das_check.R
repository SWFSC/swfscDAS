test_that("das_check no error output", {
  y.check <- das_check(system.file("das_sample.das", package = "swfscDAS"))

  exp.df <- data.frame(
    LineNum = NA, ID = NA, Description = "No errors found",
    stringsAsFactors = FALSE
  )

  expect_identical(y.check, exp.df)

  # TODO: Make das_sample_error.das with some errors to check here
})
