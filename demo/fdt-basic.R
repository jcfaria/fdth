library(fdth)

set.seed(42)
x <- rnorm(n = 1e3,
           mean = 5,
           sd = 1)

cat("\n--- Basic numeric fdt ---\n")
ft <- fdt(x)
print(ft)

cat("\n--- Alternative breaks ---\n")
print(fdt(x, breaks = "Scott"))
print(fdt(x, breaks = "FD"))

cat("\n--- Fixed number of classes ---\n")
print(fdt(x, k = 10))

cat("\n--- Custom interval boundaries ---\n")
print(fdt(x,
          start = floor(min(x)),
          end = floor(max(x) + 1),
          h = 1))

cat("\n--- Right-closed classes ---\n")
x2 <- sort(rep(1:3, 3))
print(fdt(x2,
          start = 0,
          end = 3,
          h = 1,
          right = TRUE))
