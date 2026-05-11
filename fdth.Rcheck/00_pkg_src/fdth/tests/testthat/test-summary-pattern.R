test_that("summary(fdt) accepts pattern with '%' like %.2f without Rd escapes", {
  set.seed(42)
  ft <- fdt(rnorm(120, mean = 10, sd = 2))

  out <- NULL
  capture.output({
    out <- summary(ft,
                   format.classes = TRUE,
                   pattern = "%.2f")
  })

  expect_s3_class(out, "data.frame")
  cl <- out[["Class limits"]]
  expect_type(cl, "character")
  expect_true(nzchar(paste(cl, collapse = "")))
  expect_true(any(grepl("[0-9]", cl)))
})
