library(fdth)

set.seed(2026)
x <- rnorm(1e3,
           20,
           3)
ft <- fdt(x)

# Measures computed from an fdt

# Central tendency
mean_ft <- mean(ft)
median_ft <- median(ft)
mode_ft <- mfv(ft)

mean_ft
median_ft
mode_ft

# Position (separatrices)
quartiles <- quantile(ft,
                      i = 1:3,
                      probs = seq(0,
                                  1,
                                  0.25))
deciles <- quantile(ft,
                    i = 1:9,
                    probs = seq(0,
                                1,
                                0.10))
percentiles <- quantile(ft,
                        i = c(10,
                              25,
                              50,
                              75,
                              90),
                        probs = seq(0,
                                    1,
                                    0.01))

print(quartiles)
print(deciles)
print(percentiles)

# Dispersion
var_ft <- var(ft)
sd_ft <- sd(ft)
at_ft <- amplitude(ft)
iqr_ft <- quartiles[3] - quartiles[1]
cv_ft <- 100 * sd_ft / mean_ft

at_ft
var_ft
sd_ft
iqr_ft
cv_ft

# Total range for fdt.multiple
print(amplitude(fdt(iris[, 1:4])))
