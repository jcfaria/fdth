print.fdt.quantile.multiple <- function(x, ...)
{
  for (nm in names(x)) {
    cat(nm, '\n')
    print(x[[nm]], ...)
    cat('\n')
  }

  invisible(x)
}
