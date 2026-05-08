quantile.fdt <- function(x,
                         ...,
                         i=1, 
                         probs=seq(0, 1, 0.25))
{
  fdt <- with(x,
              table)

  n <- fdt[nrow(fdt), 5]
  brk <- with(x,
              seq(breaks['start'],
                  breaks['end'],
                  breaks['h']))

  h <- as.vector(with(x,
                      breaks['h']))
  
  getQ <- function(ii)
  {
    qpos <- ii * n / (length(probs) - 1)
    
    posQ <- which(qpos <= fdt[, 5])[1]
    
    liQ <- brk[posQ]
    
    # It is important if 'posQ ' is inside of the first class
    if (posQ - 1 <= 0)
      sfaQ <- 0
    else
      sfaQ <- fdt[(posQ - 1), 5]
    
    fQ <- fdt[posQ, 2]
    
    Q <- liQ + ((qpos - sfaQ) * h) / fQ
  }
  
  res <- sapply(i,
                getQ)

  return(res)
}                        
