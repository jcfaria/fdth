mfv.fdt.cat.multiple <- function(x, ...)
{
  res <- lapply(x,
                mfv.fdt.cat)

  return(res)
}
