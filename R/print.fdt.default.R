print.fdt.default <-
  function (x, columns=1:6, round=2, format.classes=FALSE,
            pattern='%09.3e', row.names=FALSE, right=TRUE, Sweave=FALSE, ...)
  {
    res <- x[['table']]
    res <- cbind(res[, 1], round(res[, 2:6], round))[columns]
    right.tmp <- as.logical(x[['breaks']]['right'])
    if (format.classes) {
      tmp <- as.character(res[, 1])
      res[, 1] <- make.fdt.format.classes(tmp, right.tmp, pattern)}
    names(res) <- c('Class limits', 'f', 'rf', 'rf(%)', 'cf', 'cf(%)')[columns]
    if (Sweave)
      invisible(res)
    else
      print.data.frame(res, row.names=row.names, right=right, ...)
  }

