library(fdth)

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

# Theoretical curve over density
y <- rnorm(1e5,
           mean = 5,
           sd = 1)
ft2 <- fdt(y,
           k = 100)

plot(ft2,
     type = "d",
     col = heat.colors(100))
curve(dnorm(x,
            mean = 5,
            sd = 1),
      n = 1e3,
      add = TRUE,
      lwd = 3,
      col = "darkblue")

# Plot variants for fdt_cat
set.seed(321)
cats <- sample(c("A",
                 "B",
                 "C",
                 "D"),
               size = 200,
               replace = TRUE)

ft_cat <- fdt_cat(cats)

plot(ft_cat,
     type = "fb",
     main = "Frequency bar chart")

plot(ft_cat,
     type = "fp",
     main = "Frequency pie chart")

plot(ft_cat,
     type = "fd",
     main = "Frequency dotchart")

plot(ft_cat,
     type = "pa",
     main = "Pareto chart")
