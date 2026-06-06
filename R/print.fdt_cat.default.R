print.fdt_cat.default <- function(x,
                                   columns = 1:6,
                                   round = 2,
                                   row.names = FALSE,
                                   right = TRUE, ...)
{
  .fdt.format.cat(tbl = x,
                  columns = columns,
                  round.digits = round,
                  row.names = row.names,
                  right = right, ...)

  invisible(x)
}
