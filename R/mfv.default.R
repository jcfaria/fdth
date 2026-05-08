mfv.default <- function(x, ...)
{
  ux <- unique(x)
  counts <- tabulate(match(x, ux))
  max_count <- max(counts)
  res <- as.vector(ux[counts == max_count])
  return(res)
}                        
