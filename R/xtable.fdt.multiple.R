xtable.fdt.multiple <- function(x,
                                caption = NULL,
                                label = NULL,
                                align = NULL,
                                digits = NULL,
                                display = NULL, ...){

  res <- lapply(x, function(x)x$table)
  res1 <- res[names(res)!='call']
  
  res_DF <- res1

  for (i in seq_along(res1))
    res_DF[[i]] <- .fdt.xtable.prep.df(res1[[i]],
                                       math.limits = TRUE)

  attr(res_DF, "subheadings") <- attr(x, "subheadings")
  return(xtableList(res_DF,
                    caption = caption,
                    label = label,
                    align = align,
                    digits = digits,
                    display = display,
                    ...))
}

