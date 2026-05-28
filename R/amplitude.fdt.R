amplitude.fdt <- function(x, ...)
{
  with(x,
       as.numeric(breaks["end"] - breaks["start"]))
}

