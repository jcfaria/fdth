print.fdt.default <- function(x,
                              columns = 1:6,
                              round = 2,
                              format.classes = FALSE,
                              pattern = '%09.3e',
                              row.names = FALSE,
                              right = TRUE, ...)
{
  .fdt.format.num(tbl = x[['table']],
                  brk = x[['breaks']],
                  columns = columns,
                  round.digits = round,
                  format.classes = format.classes,
                  pattern = pattern,
                  row.names = row.names,
                  right = right, ...)

  invisible(x)
}
