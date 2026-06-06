xtable.fdt.quantile.multiple <- function(x,
                                         caption = NULL,
                                         label = NULL,
                                         align = NULL,
                                         digits = NULL,
                                         display = NULL, ...)
{
  res_DF <- lapply(x,
                   .fdt.xtable.prep.quantile)

  attr(res_DF, "subheadings") <- attr(x, "subheadings")

  return(xtableList(res_DF,
                    caption = caption,
                    label = label,
                    align = align,
                    digits = digits,
                    display = display,
                    ...))
}
