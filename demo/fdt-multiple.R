library(fdth)

cat("\n--- Multiple numeric variables (data.frame) ---\n")
ft_iris <- fdt(iris[, 1:4])
print(ft_iris)

cat("\n--- Group by factor column ---\n")
ft_by <- fdt(iris[, c(1, 2, 5)],
             k = 5,
             by = "Species")
print(ft_by)

cat("\n--- Matrix input ---\n")
ft_matrix <- fdt(state.x77)
print(summary(ft_matrix,
              format = TRUE,
              pattern = "%.2f"))
