library(fdth)

set.seed(2026)
x <- rnorm(1e3,
           20,
           3)
ft <- fdt(x)

## Measures computed from an fdt
# Central tendency
mean(ft)
median(ft)
mfv(ft)


# Position (separatrices)
quantile(ft,
         i = 1:3,
         probs = seq(0,
                     1,
                     0.25))
quantile(ft,
         i = 1:9,
         probs = seq(0,
                     1,
                     0.10))
quantile(ft,
         i = c(10,
               25,
               50,
               75,
               90),
         probs = seq(0,
                     1,
                     0.01))

# Dispersion
var(ft)
sd(ft)
amplitude(ft)
ta(ft) # same as amplitude(ft)

# Interquartile range
iqr_ft <- quantile(ft,
                   i = 3,
                   probs = seq(0,
                               1,
                               0.25)) -
  quantile(ft,
           i = 1,
           probs = seq(0,
                       1,
                       0.25))
iqr_ft

#Coefficient of variation
cv_ft <- 100 * sd(ft) / mean(ft)
cv_ft

# Total range for fdt.multiple
ft_by <- fdt(iris[, c(1, 2, 5)],
             by = "Species")
amplitude(ft_by)
