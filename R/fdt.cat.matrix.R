fdt.cat.matrix <- function (x,
                            sort=TRUE,
                            decreasing=TRUE, ...)
{
  stopifnot(is.matrix(x))

  res <- list()

  x <- na.omit(x)

  for (i in 1:ncol(x)) {
    m <- as.matrix(x[ ,i])

    fdt <- make.fdt.cat.simple(m,
                               sort,
                               decreasing)

    res <- c(res,
             list(fdt))
  }

  if (is.null(colnames(x)))
    nms <- paste('Column',
                 1:ncol(x),
                 sep=':')
  else
    nms <- colnames(x)

  names(res) <- nms

  class(res) <- c('fdt.cat.multiple',
                  'fdt.cat',
                  'list')

  invisible(res)
}
