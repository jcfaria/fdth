library(fdth)
library(xtable)

set.seed(123)
t1 <- fdt(rnorm(n = 1e3,
                mean = 10,
                sd = 2),
          x.round = 3)

t1x <- xtable(t1)

# Basic xtable output from fdt
print(t1x,
      include.rownames = FALSE,
      sanitize.text.function = function(x) x)

# Class limits with two decimal places
clim <- t1$table[1]
clim1 <- sapply(clim, as.character)
right <- t1$breaks[4]
pattern <- "%05.2f"
clim2 <- fdth:::make.fdt.format.classes(clim1,
                                        right,
                                        pattern)
clim3 <- format(sapply(clim2,
                       function(x) paste0("$", x, "$")),
                justify = "left")
clim3[1] <- paste0("  ", clim3[1])
t2x <- t1x
t2x[, 1] <- clim3
print(t2x,
      include.rownames = FALSE,
      sanitize.text.function = function(x) x)

# fdt.multiple with subheadings
t_multi <- fdt(iris[, c(1:2, 5)],
               by = "Species")
attr(t_multi, "subheadings") <- paste0("Variable = ", names(t_multi))
print(xtable(t_multi),
      table.placement = "H",
      include.rownames = FALSE,
      sanitize.text.function = function(x) x)

# longtable option for wide grouped tables
t_multi_x <- xtable(t_multi)
print(t_multi_x,
      table.placement = "H",
      include.rownames = FALSE,
      sanitize.text.function = function(x) x,
      tabular.environment = "longtable",
      floating = FALSE)

# Categorical table to LaTeX
t_cat <- fdt_cat(sample(LETTERS[1:3],
                        replace = TRUE,
                        size = 30))
print(xtable(t_cat),
      table.placement = "H",
      include.rownames = FALSE,
      sanitize.text.function = function(x) x)

# fdt_cat.multiple with subheadings
mdf_cat <- data.frame(c1 = sample(LETTERS[1:3],
                                  1e2,
                                  rep = TRUE),
                      c2 = as.factor(sample(1:5,
                                            1e2,
                                            rep = TRUE)),
                      stringsAsFactors = TRUE)
t_cat_multi <- fdt_cat(mdf_cat)
attr(t_cat_multi, "subheadings") <- paste0("Variable = ", names(t_cat_multi))
print(xtable(t_cat_multi),
      table.placement = "H",
      include.rownames = FALSE,
      sanitize.text.function = function(x) x)

# Quantiles estimated from grouped data
ft <- fdt(rnorm(n = 1e3,
                mean = 10,
                sd = 2))
q_ft <- quantile(ft,
                 i = 1:3)
print(xtable(q_ft),
      include.rownames = FALSE,
      sanitize.text.function = function(x) x)

ft_multi <- fdt(iris[, 1:4])
q_multi <- quantile(ft_multi,
                    i = 1:3)
attr(q_multi, "subheadings") <- names(q_multi)
print(xtable(q_multi),
      include.rownames = FALSE,
      sanitize.text.function = function(x) x)

# xtable after print() returns the original fdt object invisibly
print(xtable(print(ft_multi)),
      include.rownames = FALSE,
      sanitize.text.function = function(x) x)
