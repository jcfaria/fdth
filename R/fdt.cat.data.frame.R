fdt.cat.data.frame <- function (x,
                                sort=TRUE,
                                decreasing=TRUE, ...)
{
  stopifnot(is.data.frame(x))

  x <- na.omit(x)

  res <- make.fdt.cat.multiple(x,
                               sort,
                               decreasing)

  class(res) <- c('fdt.cat.multiple',
                  'fdt.cat',
                  'list')

  invisible(res)
}

