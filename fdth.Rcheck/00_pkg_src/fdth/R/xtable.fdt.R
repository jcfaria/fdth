xtable.fdt <- function(x,caption = NULL, label = NULL, align = NULL,
                       digits = NULL, display = NULL, auto = FALSE,...){
  res_DF <- x$table
  newclass1 <- res_DF[,1]
  newclass2 <- gsub("\\[","$[",newclass1)
  newclass3 <- gsub("\\)",")$",newclass2)
  res_DF[,1] <- newclass3
  newnames <- names(res_DF)
  newnames1 <- gsub("\\%","\\\\%",newnames)
  names(res_DF) <- newnames1

  return(xtable(res_DF,caption = caption, label = label, align = align,
                digits = digits, display = display, auto = auto, ...))
}
