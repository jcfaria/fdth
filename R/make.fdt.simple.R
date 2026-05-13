make.fdt.simple <- function (x,
                             start,
                             end,
                             h,
                             right)

{
  f <- table(cut(x,
                 br=seq(start,
                        end,
                        h),
                 right=right,
                 dig.lab=nchar(as.character(round(max(end),
                                                  2)))))    # Absolute freq.
  rf  <- as.numeric(f/sum(f))                              # Relative freq
  rfp <- as.numeric(100*(f/sum(f)))                        # Relative freq, %
  cf  <- as.numeric(cumsum(f))                             # Cumulative freq
  cfp <- as.numeric(100*(cumsum(f/sum(f))))                # Cumulative freq, %

  res <- data.frame(f,                                      # Make final table
                    rf,
                    rfp,
                    cf,
                    cfp)

  names(res) <- c('Class limits',
                  'f',
                  'rf',
                  'rf(%)',
                  'cf',
                  'cf(%)')

  return(res)
}
