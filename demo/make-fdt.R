library(fdth)

set.seed(10)
x <- rnorm(1e3,
           20,
           2)

# Rebuild numeric fdt from frequencies
ft_ref <- fdt(x)

summary(ft_ref,
        columns = c(1, 2, 4, 6),
        format = TRUE,
        pattern = "%.3f")

ft_new <- make.fdt(f = ft_ref$table$f,
                   start = ft_ref$breaks["start"],
                   end = ft_ref$breaks["end"])

summary(ft_new,
        columns = c(1, 2, 4, 6),
        format = TRUE,
        pattern = "%.3f")

# Rebuild categorical fdt from frequencies
fruits <- sample(c("apple", "banana", "cherry", "date"),
                 size=150,
                 replace = TRUE)

ftc_ref <- fdt_cat(fruits)

summary(ftc_ref,
        columns = c(1, 2, 4, 6))

ftc_new <- make.fdt_cat(f = ftc_ref$f,
                        categories = ftc_ref$Category)
summary(ftc_new,
        columns = c(1, 2, 4, 6))
