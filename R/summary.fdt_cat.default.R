summary.fdt_cat.default <- function(object,
                                     columns=1:6,
                                     round=2,
                                     row.names=FALSE,
                                     right=TRUE, ...)
{
  invisible(
    .fdt.format.cat(tbl=object,
                    columns=columns,
                    round.digits=round,
                    row.names=row.names,
                    right=right, ...)
  )
}
