amplitude.default <- function(x, ...)
{
  r <- range(x, ...)
  r[2] - r[1]
}

