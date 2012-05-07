make.fdt.simple <-
  function (x, start, end, h, right)
  {
    f   <- table(cut(x, br=seq(start, end, h), right=right)) # Absolut freq.
    rf  <- as.numeric(f/length(x))                           # Relative freq
    rfP <- as.numeric(100*(f/length(x)))                     # Relative freq, %
    cf  <- as.numeric(cumsum(f))                             # Cumulative freq
    cfP <- as.numeric(100*(cumsum(f/length(x))))             # Cumulative freq, %
    res <- data.frame(f, rf, rfP, cf, cfP)                   # Make final table
    names(res) <- c('Class limits', 'f', 'rf', 'rf(%)', 'cf', 'cf(%)')
    return(res)
  }

