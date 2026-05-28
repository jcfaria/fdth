library(fdth)

set.seed(42)
x <- rnorm(n = 1e3,
           mean = 5,
           sd = 1)

# Basic numeric fdt
ft <- fdt(x)
print(ft)

# Alternative breaks
print(fdt(x, breaks = "Scott"))
print(fdt(x, breaks = "FD"))

# Fixed number of classes
print(fdt(x, k = 10))

# Custom interval boundaries
print(fdt(x,
          start = floor(min(x)),
          end = floor(max(x) + 1),
          h = 1))

# Right-closed classes
x2 <- sort(rep(1:3, 3))
print(fdt(x2,
          start = 0,
          end = 3,
          h = 1,
          right = TRUE))
