mean.fdt.multiple <- function(x, ...)
{
  xx1 <- x[names(x)!='call']
  class(xx1) <- 'fdt.multiple'
  
  res <- lapply(xx1,
                mean.fdt, ...)

  return(res)
}
