library(fdth)

set.seed(10)
x <- rnorm(1e3,
           20,
           2)

cat("\n--- Rebuild numeric fdt from frequencies ---\n")
ft_ref <- fdt(x)
ft_new <- make.fdt(f = ft_ref$table$f,
                   start = ft_ref$breaks["start"],
                   end = ft_ref$breaks["end"])
print(summary(ft_new,
              format = TRUE,
              pattern = "%.3f"))

cat("\n--- Rebuild categorical fdt from frequencies ---\n")
fruits <- sample(c("apple", "banana", "cherry", "date"),
                 150,
                 replace = TRUE)
ft_cat <- fdt_cat(fruits)
ft_new_cat <- make.fdt_cat(f = ft_cat$f,
                           categories = ft_cat$Category)
print(ft_new_cat)
