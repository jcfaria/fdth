fdt.cat.default <- function (x,
                             sort=TRUE,
                             decreasing=TRUE, ...)
{
  x <- na.omit(x)

  res <- make.fdt.cat.simple(x,
                             sort,
                             decreasing)

  class(res) <- c('fdt.cat.default',
                  'fdt.cat',
                  'data.frame')

  invisible(res)
}  
