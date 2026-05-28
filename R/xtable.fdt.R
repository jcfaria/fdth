xtable.fdt <- function(x,
                       caption = NULL,
                       label = NULL,
                       align = NULL,
                       digits = NULL,
                       display = NULL,
                       auto = FALSE, ...){
  res_DF <- .fdt.xtable.prep.df(x$table,
                                math.limits = TRUE)

  return(xtable(res_DF,
                caption = caption,
                label = label,
                align = align,
                digits = digits,
                display = display,
                auto = auto, ...))
}
