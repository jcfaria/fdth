print.fdt.cat.multiple <- function (x,
                                    columns=1:6,
                                    round=2,
                                    row.names=FALSE, 
                                    right=TRUE, ...)
{
  tnames <- names(x)

  for (i in 1:length(tnames)) {
    res <- x[[tnames[i]]]

    cat(tnames[i], '\n')

    print.data.frame(res[, columns],
                     row.names=row.names,
                     right=right, ...)

    cat('\n')}
}

