mfv.fdt.cat.multiple <- function(x, ...)
{
 
 if(class(x)[1]=='fdt.cat.multiple' | class(x)[1] == 'fdt.cat'){
 res <- list()

 for(i in 1:length(x)){

  y <- x[[i]][[1]]
  class(y) <- c('fdt.cat.multiple',
                'fdt.cat',
                'data.frame')
  res[[i]] <- mfv.fdt.cat(y)

 }


}
 else {

  res <- lapply(x,
                mfv.fdt.cat)

 }

return(res)
}
