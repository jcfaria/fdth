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
ta(ft) # same as amplitude(ft)

# Quartiles
print(quantile(ft)) # first quartile

print(quantile(ft,
               i = 1:3,
               probs = seq(0,
                           1,
                           0.25)))

# Deciles
print(quantile(ft,
               i = 1:9,
               probs = seq(0,
                           1,
                           0.1)))

# Percentiles
print(quantile(ft,
               i = c(10,
                     25,
                     50,
                     75,
                     90),
               probs = seq(0,
                           1,
                           0.01)))
