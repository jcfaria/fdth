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

