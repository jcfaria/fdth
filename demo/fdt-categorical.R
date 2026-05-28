library(fdth)

set.seed(7)
x <- sample(x = letters[1:5],
            size = 5e2,
            rep = TRUE)

# Basic categorical fdt_cat
summary(fdt_cat(x),
        columns = c(1, 2, 4, 6),
        format = TRUE,
        pattern = '%.2f')

# Preserve original order (sort = FALSE)
summary(fdt_cat(x, sort = FALSE),
        columns = c(1, 2, 4, 6),
        format = TRUE,
        pattern = '%.2f')

# Categorical data.frame (multiple outputs)
mdf <- data.frame(c1 = sample(LETTERS[1:3],
                              1e2,
                              rep = TRUE),
                  c2 = as.factor(sample(1:10,
                                        1e2,
                                        rep = TRUE)),
                  n1 = c(NA,
                         NA,
                         rnorm(96,
                               10,
                               1),
                         NA,
                         NA),
                  n2 = rnorm(100,
                             60,
                             4),
                  n3 = rnorm(100,
                             50,
                             4),
                  stringsAsFactors = TRUE)

print(fdt_cat(mdf))
print(fdt_cat(mdf, dec = FALSE))
print(fdt_cat(mdf, sort = FALSE))
print(fdt_cat(mdf, by = "c1"))
