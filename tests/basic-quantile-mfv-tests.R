library(fdth)

set.seed(123)

# quantile.fdt should support vector i and be equivalent
# to the legacy loop approach.
x <- rnorm(200,
           mean=10,
           sd=2)
fx <- fdt(x)

q_vec <- quantile(fx,
                  i=1:3,
                  probs=seq(0,
                            1,
                            0.25))

q_loop <- numeric()
for (ii in 1:3)
  q_loop[ii] <- quantile(fx,
                         i=ii,
                         probs=seq(0,
                                   1,
                                   0.25))

stopifnot(length(q_vec) == 3)
stopifnot(isTRUE(all.equal(as.numeric(q_vec),
                           as.numeric(q_loop))))

# quantile.fdt input validation
stopifnot(inherits(try(quantile(fx,
                                i=c(0, 1),
                                probs=seq(0, 1, 0.25)),
                      silent=TRUE),
                  "try-error"))

stopifnot(inherits(try(quantile(fx,
                                i=1.5,
                                probs=seq(0, 1, 0.25)),
                      silent=TRUE),
                  "try-error"))

# mfv.default multimodal behavior
mv <- mfv(c(1, 1, 2, 2, 3))
stopifnot(identical(as.numeric(mv),
                    c(1, 2)))
