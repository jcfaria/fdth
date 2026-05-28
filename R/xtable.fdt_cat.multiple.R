xtable.fdt_cat.multiple <- function(x,
                                    caption = NULL,
                                    label = NULL,
                                    align = NULL,
                                    digits = NULL,
                                    display = NULL, ...){

  res1 <- x[names(x) != "call"]
  res <- lapply(res1,
                .fdt.xtable.cat.item)
  res <- lapply(res,
                .fdt.xtable.prep.df,
                math.limits = FALSE)
  attr(res, "subheadings") <- attr(x, "subheadings")
  return(xtableList(res,
                    caption = caption,
                    label = label,
                    align = align,
                    digits = digits,
                    display = display,
                    ...))
}

