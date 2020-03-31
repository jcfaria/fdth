xtable.fdt_cat.multiple <- function(x,caption = NULL, label = NULL, align = NULL,
                                digits = NULL, display = NULL, ...){

  res <- lapply(x,function(x)x$table)
  attr(res,"subheadings") <- attr(x,"subheadings")
  return(xtableList(res,
                    caption=caption,
                    label=label,
                    align=align,
                    digits=digits,
                    display=display,
                    ...))
}

