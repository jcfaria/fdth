library(fdth)

# NOTE: column names 'val1' / 'val2' are intentional.
# fdt.data.frame uses 'x' as its formal parameter and calls subset() with an
# expression that contains 'x'.  subset() evaluates conditions in the data
# frame's environment first, so a column literally called 'x' would shadow the
# formal parameter and cause an empty subset.  Using distinct names avoids
# that pre-existing package limitation.
set.seed(7)
df <- data.frame(
  grp  = factor(sample(c("A", "B"), 100, replace = TRUE)),
  val1 = rnorm(100, 10, 2),
  val2 = rnorm(100, 50, 5)
)

test_that("fdt on a multi-numeric data frame returns fdt.multiple", {
  ft <- fdt(df[, c("val1", "val2")])
  expect_s3_class(ft, "fdt.multiple")
})

test_that("fdt.multiple slots are named after the numeric columns", {
  ft  <- fdt(df[, c("val1", "val2")])
  nms <- names(ft)[names(ft) != "call"]
  expect_setequal(nms, c("val1", "val2"))
})

test_that("fdt with by argument returns fdt.multiple with level.var names", {
  ft  <- fdt(df, by = "grp")
  nms <- names(ft)[names(ft) != "call"]
  # Each name should be of the form "<level>.<variable>"
  expect_true(all(grepl("^[AB]\\.", nms)))
})

test_that("mean on fdt.multiple returns a named list", {
  ft  <- fdt(df[, c("val1", "val2")])
  res <- mean(ft)
  expect_type(res, "list")
  expect_named(res, c("val1", "val2"))
})

test_that("print.fdt.multiple runs without error", {
  ft <- fdt(df[, c("val1", "val2")])
  expect_no_error(capture.output(print(ft)))
})

test_that("summary.fdt.multiple runs without error", {
  ft <- fdt(df[, c("val1", "val2")])
  expect_no_error(capture.output(summary(ft)))
})
