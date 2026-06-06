library(fdth)
library(xtable)

test_that("xtable.fdt keeps first class line aligned", {
  set.seed(123)
  tb <- fdt(rnorm(n = 1e3,
                  mean = 10,
                  sd = 2),
            x.round = 3)
  tx <- xtable(tb)

  out <- capture.output(print(tx,
                              include.rownames = FALSE,
                              sanitize.text.function = function(x) x))

  hline_idx <- grep("\\\\hline", out)[2]
  first_data_line <- out[hline_idx + 1]
  second_data_line <- out[hline_idx + 2]

  expect_true(startsWith(first_data_line, "  $"))
  expect_true(startsWith(second_data_line, "  $"))
})

test_that("xtable.fdt.multiple keeps class lines aligned", {
  tb <- fdt(iris[, c(1:2, 5)],
            by = "Species")
  attr(tb, "subheadings") <- paste0("Variable = ", names(tb))
  tx <- xtable(tb)

  out <- capture.output(print(tx,
                              include.rownames = FALSE,
                              sanitize.text.function = function(x) x))

  sh_idx <- grep("Variable = ", out, fixed = TRUE)
  first_lines <- out[sh_idx + 1]

  expect_true(length(first_lines) > 0)
  expect_true(all(startsWith(first_lines, "  $")))
})

test_that("xtable.fdt_cat keeps first category line aligned", {
  set.seed(321)
  tb <- fdt_cat(sample(LETTERS[1:3],
                       replace = TRUE,
                       size = 30))
  tx <- xtable(tb)

  out <- capture.output(print(tx,
                              include.rownames = FALSE,
                              sanitize.text.function = function(x) x))

  hline_idx <- grep("\\\\hline", out)[2]
  first_data_line <- out[hline_idx + 1]
  second_data_line <- out[hline_idx + 2]

  expect_true(startsWith(first_data_line, "  "))
  expect_true(startsWith(second_data_line, "  "))
})

test_that("xtable.fdt_cat.multiple keeps category lines aligned", {
  mdf <- data.frame(c1 = sample(LETTERS[1:3],
                                1e2,
                                rep = TRUE),
                    c2 = as.factor(sample(1:5,
                                          1e2,
                                          rep = TRUE)),
                    stringsAsFactors = TRUE)
  tb <- fdt_cat(mdf)
  attr(tb, "subheadings") <- paste0("Variable = ", names(tb))
  tx <- xtable(tb)

  out <- capture.output(print(tx,
                              include.rownames = FALSE,
                              sanitize.text.function = function(x) x))

  sh_idx <- grep("Variable = ", out, fixed = TRUE)
  first_lines <- out[sh_idx + 1]

  expect_true(length(first_lines) > 0)
  expect_true(all(startsWith(first_lines, "  ")))
})

test_that("print.fdt.quantile hides class attribute", {
  set.seed(123)
  tb <- fdt(rnorm(n = 200,
                  mean = 10,
                  sd = 2))
  qq <- quantile(tb,
                 i = 1:3)
  expect_s3_class(qq, "fdt.quantile")

  out <- capture.output(print(qq))
  expect_false(any(grepl("^attr\\(,", out)))
  expect_true(any(grepl("25%", out)))
})

test_that("xtable.fdt.quantile exports quantile labels", {
  set.seed(123)
  tb <- fdt(rnorm(n = 200,
                  mean = 10,
                  sd = 2))
  qq <- quantile(tb,
                 i = 1:3)
  expect_s3_class(qq, "fdt.quantile")

  tx <- xtable(qq)
  out <- capture.output(print(tx,
                              include.rownames = FALSE,
                              sanitize.text.function = function(x) x))

  expect_true(any(grepl("25\\\\%", out)))
  expect_true(any(grepl("Quantile", out)))
})

test_that("xtable.fdt.quantile.multiple exports one table per variable", {
  tb <- fdt(iris[, 1:4])
  qq <- quantile(tb,
                 i = 1:3)
  expect_s3_class(qq, "fdt.quantile.multiple")

  attr(qq, "subheadings") <- names(qq)
  tx <- xtable(qq)

  out <- capture.output(print(tx,
                              include.rownames = FALSE,
                              sanitize.text.function = function(x) x))

  expect_true(any(grepl("Sepal.Length", out)))
  expect_true(any(grepl("Petal.Width", out)))
})

test_that("print.fdt.multiple returns fdt.multiple invisibly for xtable", {
  tb <- fdt(iris[, 1:4])
  printed <- print(tb)

  expect_s3_class(printed, "fdt.multiple")
  expect_no_error(xtable(printed))
})

