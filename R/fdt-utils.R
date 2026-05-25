## Internal helpers shared by print and summary methods.
## Not exported; prefixed with '.' to signal internal use.

.fdt.format.num <- function(tbl,
                            brk,
                            columns,
                            round.digits,
                            format.classes,
                            pattern,
                            row.names,
                            right,
                            ...)
{
  res <- cbind(tbl[, 1], round(tbl[, 2:6], round.digits))[columns]

  right.tmp <- as.logical(brk['right'])

  if (format.classes) {
    tmp      <- as.character(res[, 1])
    res[, 1] <- make.fdt.format.classes(tmp,
                                        right.tmp,
                                        pattern)
  }

  names(res) <- c('Class limits',
                  'f',
                  'rf',
                  'rf(%)',
                  'cf',
                  'cf(%)')[columns]

  print.data.frame(res,
                   row.names = row.names,
                   right = right, ...)
}

.fdt.format.cat <- function(tbl,
                            columns,
                            round.digits,
                            row.names,
                            right,
                            ...)
{
  res <- cbind(tbl[, 1], round(tbl[, 2:6], round.digits))[columns]

  names(res) <- c('Category',
                  'f',
                  'rf',
                  'rf(%)',
                  'cf',
                  'cf(%)')[columns]

  print.data.frame(res,
                   row.names = row.names,
                   right = right, ...)
}
