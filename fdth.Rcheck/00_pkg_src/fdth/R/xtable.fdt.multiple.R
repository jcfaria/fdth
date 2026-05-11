xtable.fdt.multiple <- function(x,caption = NULL, label = NULL, align = NULL,
                                digits = NULL, display = NULL, ...){

  res <- lapply(x,function(x)x$table)
  res1 <- res[names(res)!='call']
  
  newclass1 <- lapply(res1,function(x)x[,1])
  newclass2 <- lapply(newclass1,function(x) gsub("\\[","$[",x)) 
  newclass3 <- lapply(newclass2,function(x) gsub("\\)",")$",x))  

  res_DF <- res1

  for(i in 1:length(res1)){
   res_DF[[i]][,1] <- newclass3[[i]]
  }

  newnames <- lapply(res_DF,function(x)names(x))
  newnames1 <- lapply(newnames,function(x) gsub("\\%","\\\\%",x)) 
  
  for(i in 1:length(res_DF)){
   names(res_DF[[i]]) <- newnames1[[i]]
  }

  attr(res_DF,"subheadings") <- attr(x,"subheadings")
  return(xtableList(res_DF,
                    caption=caption,
                    label=label,
                    align=align,
                    digits=digits,
                    display=display,
                    ...))
}

