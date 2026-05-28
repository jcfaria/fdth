library(fdth)
library(xtable)

set.seed(33)
x <- rnorm(1e3,
           20,
           2)
ft <- fdt(x)

# Plot variants for fdt
plot(ft,
     type = "fh",
     main = "Frequency histogram")
plot(ft,
     type = "fp",
     main = "Frequency polygon")
plot(ft,
     type = "d",
     main = "Density")
plot(ft,
     type = "cfpp",
     main = "Cumulative frequency (%) polygon")

# xtable from fdt
t1x <- xtable(ft)
print(t1x,
      include.rownames = FALSE,
      sanitize.text.function = function(z) z)

# xtable from grouped object (by)
t_multi <- fdt(iris,
               by = "Species")
attr(t_multi, "subheadings") <- paste0("Variable = ", names(t_multi))
print(xtable(t_multi),
      table.placement = "H")
