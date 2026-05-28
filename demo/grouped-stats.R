library(fdth)

set.seed(123)
x <- rnorm(1e3,
           20,
           2)
ft <- fdt(x)

# Grouped statistics from an fdt object
mean(ft)
median(ft)
mfv(ft)
var(ft)
sd(ft)
amplitude(ft)

# Quartiles
print(quantile(ft))

# Deciles
print(quantile(ft,
               i = 1:9,
               probs = seq(0,
                           1,
                           0.1)))
