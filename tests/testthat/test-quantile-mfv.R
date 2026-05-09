test_that("quantile.fdt matches legacy loop for vector i", {
  set.seed(123)
  x <- rnorm(200, mean = 10, sd = 2)
  fx <- fdt(x)

  q_vec <- quantile(fx,
                    i = 1:3,
                    probs = seq(0, 1, 0.25))

  q_loop <- numeric()
  for (ii in 1:3) {
    q_loop[ii] <- quantile(fx,
                           i = ii,
                           probs = seq(0, 1, 0.25))
  }

  expect_length(q_vec, 3)
  expect_equal(as.numeric(q_vec), as.numeric(q_loop))
})

test_that("quantile.fdt rejects invalid i", {
  set.seed(123)
  fx <- fdt(rnorm(200, mean = 10, sd = 2))

  expect_error(quantile(fx,
                        i = c(0, 1),
                        probs = seq(0, 1, 0.25)))

  expect_error(quantile(fx,
                        i = 1.5,
                        probs = seq(0, 1, 0.25)))
})

test_that("mfv.default returns all modes when multimodal", {
  mv <- mfv(c(1, 1, 2, 2, 3))
  expect_equal(as.numeric(mv), c(1, 2))
})
