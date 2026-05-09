fdt_var.fdt.multiple <- function(x, ...)
{
  xx1 <- x[names(x)!='call']
  class(xx1) <- 'fdt.multiple'
  
  res <- lapply(xx1,
                fdt_var.fdt)

  return(res)
}
