library(fdth)

set.seed(2026)
x <- rnorm(1e3,
           20,
           3)
ft <- fdt(x)

cat("\n===============================\n")
cat("Measures computed from an fdt\n")
cat("===============================\n")

cat("\n--- Central tendency ---\n")
mean_ft <- mean(ft)
median_ft <- median(ft)
mode_ft <- mfv(ft)

cat("Mean   :", mean_ft, "\n")
cat("Median :", median_ft, "\n")
cat("Mode   :", paste(mode_ft, collapse = ", "), "\n")

cat("\n--- Position (separatrices) ---\n")
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
                        i = c(10, 25, 50, 75, 90),
                        probs = c(0.10, 0.25, 0.50, 0.75, 0.90))

cat("\nQuartiles (Q1, Q2, Q3):\n")
print(quartiles)
cat("\nDeciles (D1 ... D9):\n")
print(deciles)
cat("\nSelected percentiles (P10, P25, P50, P75, P90):\n")
print(percentiles)

cat("\n--- Dispersion ---\n")
var_ft <- var(ft)
sd_ft <- sd(ft)
at_ft <- amplitude(ft)
iqr_ft <- quartiles[3] - quartiles[1]
cv_ft <- 100 * sd_ft / mean_ft

cat("Total range (class limits):", at_ft, "\n")
cat("Variance               :", var_ft, "\n")
cat("Standard deviation     :", sd_ft, "\n")
cat("Interquartile range    :", iqr_ft, "\n")
cat("Coefficient of variation (%):", cv_ft, "\n")

cat("\n--- Total range for fdt.multiple ---\n")
print(amplitude(fdt(iris[, 1:4])))
