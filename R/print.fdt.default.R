# Internal helper: format the numeric fdt table and send it to the console.
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
  res <- cbind(tbl[, 1],
               round(tbl[, 2:6],
                     round.digits))[columns]

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
                   row.names=row.names,
                   right=right, ...)
}

print.fdt.default <- function(x,
                               columns=1:6,
                               round=2,
                               format.classes=FALSE,
                               pattern='%09.3e',
                               row.names=FALSE,
                               right=TRUE, ...)
{
  .fdt.format.num(tbl=x[['table']],
                  brk=x[['breaks']],
                  columns=columns,
                  round.digits=round,
                  format.classes=format.classes,
                  pattern=pattern,
                  row.names=row.names,
                  right=right, ...)
}
