library(fdth)

set.seed(123)
x <- rnorm(1e3,
           20,
           2)
ft <- fdt(x)

cat("\n--- Grouped statistics from fdt object ---\n")
cat("mean   :", mean(ft), "\n")
cat("median :", median(ft), "\n")
cat("mode   :", paste(mfv(ft), collapse = ", "), "\n")
cat("var    :", var(ft), "\n")
cat("sd     :", sd(ft), "\n")

cat("\n--- Quartiles ---\n")
print(quantile(ft))

cat("\n--- Deciles ---\n")
print(quantile(ft,
               i = 1:9,
               probs = seq(0,
                           1,
                           0.1)))
