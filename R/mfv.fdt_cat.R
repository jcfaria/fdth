mfv.fdt_cat <- function(x, ...)
{
  fdt <- x

  y <- fdt[, 2]

  posMFV <- which(y == max(y))

  res <- fdt[posMFV, 2]

  names(res) <- fdt[posMFV, 1]

  return(res)
}
