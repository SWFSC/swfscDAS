test_that("Arguments are passed correctly between _process and _read", {
  y <- system.file("das_sample.das", package = "swfscDAS")

  y.read <- das_read(y, skip = 20)
  y.proc <- das_process(y.read, reset.effort = FALSE)

  y.proc2 <- das_process(y, skip = 20, reset.effort = FALSE, tmp = NA)

  expect_equal(y.proc, y.proc2)
})
