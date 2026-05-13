summary.fdt.default <- function(object,
                                 columns=1:6,
                                 round=2,
                                 format.classes=FALSE,
                                 pattern='%09.3e',
                                 row.names=FALSE,
                                 right=TRUE, ...)
{
  invisible(
    .fdt.format.num(tbl=object[['table']],
                    brk=object[['breaks']],
                    columns=columns,
                    round.digits=round,
                    format.classes=format.classes,
                    pattern=pattern,
                    row.names=row.names,
                    right=right, ...)
  )
}
