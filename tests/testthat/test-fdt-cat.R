library(fdth)

set.seed(99)
x_chr <- sample(c("apple", "banana", "cherry", "apple", "banana",
                   "apple"), 120, replace = TRUE)
x_fac <- factor(x_chr)

test_that("fdt_cat returns correct class for character input", {
  ft <- fdt_cat(x_chr)
  expect_s3_class(ft, "fdt_cat.default")
})

test_that("fdt_cat result has the expected column names", {
  ft <- fdt_cat(x_chr)
  expect_named(ft, c("Category", "f", "rf", "rf(%)", "cf", "cf(%)"))
})

test_that("fdt_cat absolute frequencies sum to n", {
  ft <- fdt_cat(x_chr)
  expect_equal(sum(ft$f), length(x_chr))
})

test_that("fdt_cat relative frequencies (%) sum to 100", {
  ft <- fdt_cat(x_chr)
  expect_equal(sum(ft[["rf(%)"]]), 100, tolerance = 1e-8)
})

test_that("fdt_cat sorts by frequency descending by default", {
  ft <- fdt_cat(x_chr)
  expect_true(ft$f[1] >= ft$f[2])
  expect_true(ft$f[2] >= ft$f[3])
})

test_that("fdt_cat with sort=FALSE preserves natural factor-level order", {
  ft <- fdt_cat(x_fac, sort = FALSE)
  expect_equal(as.character(ft$Category), levels(x_fac))
})

test_that("fdt_cat accepts factor input", {
  ft <- fdt_cat(x_fac)
  expect_s3_class(ft, "fdt_cat.default")
  expect_equal(sum(ft$f), length(x_fac))
})

test_that("print.fdt_cat.default runs without error", {
  ft <- fdt_cat(x_chr)
  expect_no_error(capture.output(print(ft)))
})

test_that("summary.fdt_cat.default returns a data frame invisibly", {
  ft  <- fdt_cat(x_chr)
  out <- NULL
  capture.output(out <- summary(ft))
  expect_s3_class(out, "data.frame")
})
