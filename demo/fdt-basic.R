library(fdth)

set.seed(42)
x <- rnorm(n = 1e3,
           mean = 5,
           sd = 1)

# Basic numeric fdt
ft <- fdt(x)
str(ft) 
summary(ft, 
        columns = c(1, 2, 4, 6), # select columns to show
        format = TRUE,           # format the numbers
        pattern = '%.2f')        # use a pattern to format the numbers

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
print(fdt(sort(rep(1:3, 3)),
          start = 0,
          end = 3,
          h = 1,
          right = TRUE))
