library(fdth)

test_that("mean.fdt returns a single numeric close to sample mean", {
  set.seed(42)
  x  <- rnorm(500, mean = 20, sd = 3)
  fx <- fdt(x)
  m  <- mean(fx)
  expect_length(m, 1)
  expect_true(is.numeric(m))
  expect_true(abs(m - mean(x)) < 0.5)
})

test_that("median.fdt returns a single numeric close to sample median", {
  set.seed(42)
  x  <- rnorm(500, mean = 20, sd = 3)
  fx <- fdt(x)
  md <- median(fx)
  expect_length(md, 1)
  expect_true(is.numeric(md))
  expect_true(abs(md - median(x)) < 1)
})

test_that("var.fdt returns a positive numeric roughly agreeing with sample var", {
  set.seed(42)
  x  <- rnorm(500, mean = 20, sd = 3)
  fx <- fdt(x)
  v  <- var(fx)
  expect_length(v, 1)
  expect_true(is.numeric(v))
  expect_gt(v, 0)
  # grouped-data variance approximation: within 20 % of the sample variance
  expect_true(abs(v - var(x)) / var(x) < 0.20)
})

test_that("sd.fdt equals sqrt of var.fdt", {
  set.seed(42)
  x  <- rnorm(500, mean = 20, sd = 3)
  fx <- fdt(x)
  expect_equal(sd(fx), sqrt(var(fx)))
})

test_that("var.fdt is 0 when all values fall in one class", {
  # All identical values collapse to a single class; grouped variance is 0.
  x  <- rep(5, 50)
  fx <- fdt(x, start = 4, end = 6, h = 2)
  expect_equal(var(fx), 0)
})
