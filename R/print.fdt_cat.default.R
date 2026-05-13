# Internal helper: format the categorical fdt table and send it to the console.
.fdt.format.cat <- function(tbl,
                             columns,
                             round.digits,
                             row.names,
                             right,
                             ...)
{
  res <- cbind(tbl[, 1],
               round(tbl[, 2:6],
                     round.digits))[columns]

  names(res) <- c('Category',
                  'f',
                  'rf',
                  'rf(%)',
                  'cf',
                  'cf(%)')[columns]

  print.data.frame(res,
                   row.names=row.names,
                   right=right, ...)
}

print.fdt_cat.default <- function(x,
                                   columns=1:6,
                                   round=2,
                                   row.names=FALSE,
                                   right=TRUE, ...)
{
  .fdt.format.cat(tbl=x,
                  columns=columns,
                  round.digits=round,
                  row.names=row.names,
                  right=right, ...)
}
