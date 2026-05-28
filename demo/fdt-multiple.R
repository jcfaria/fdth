library(fdth)

# Multiple numeric variables (data.frame)
ft_iris <- fdt(iris[, 1:4])
print(ft_iris)

# Group by factor column
ft_by <- fdt(iris[, c(1, 2, 5)],
             k = 5,
             by = "Species")
print(ft_by)

# Matrix input
ft_matrix <- fdt(state.x77)
print(summary(ft_matrix,
              format = TRUE,
              pattern = "%.2f"))
