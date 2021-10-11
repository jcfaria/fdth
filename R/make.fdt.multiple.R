make.fdt.multiple <- function (x,
                               k,
                               breaks=c('Sturges', 'Scott', 'FD'),
                               right,
                               na.rm)
{
  #x <- na.omit(x)

  # Built-in functions adapted from grDevices package to accept 'na.rm' argument
  nclass.Scott_local <-
  function (x,
            na.rm=FALSE)
  {
      h <- 3.5 * sqrt(stats::var(x,
                                 na.rm=na.rm)) * length(x)^(-1/3)
      if (h > 0)
          max(1, ceiling(diff(range(x,
                                    na.rm=na.rm))/h))
      else 1L
  }


  nclass.FD_local <-
  function (x,
            na.rm=FALSE)
  {
      h <- 2 * stats::IQR(x. <- signif(x,
                                       digits = 5),
                          na.rm=na.rm)
      if (h == 0) {
          x. <- sort(x.)
          al <- 1/4
          al.min <- 1/512
          while (h == 0 && (al <- al/2) >= al.min)
            h <- diff(stats::quantile(x.,
                                      c(al, 1 - al),
                                      names = FALSE,
                                      na.rm=na.rm))/(1 - 2 * al)
      }
      if (h == 0)
          h <- 3.5 * sqrt(stats::var(x,
                                     na.rm=na.rm))
      if (h > 0)
          ceiling(diff(range(x,
                             na.rm=na.rm))/h * length(x)^(1/3))
      else 1L
  }

  # User defines only x and/or 'breaks'
  if (missing(k)) {
    brk <- match.arg(breaks)

    switch (brk,
            Sturges = (k <- nclass.Sturges(x)),

            Scott   = if (any(is.na(x)) & na.rm == FALSE)
                        stop('The data has <NA> values and na.rm=FALSE by default.')
                      else
                        (k <- nclass.Scott_local(x,
                                                 na.rm=na.rm)),

            FD      = if (any(is.na(x)) & na.rm == FALSE)
                        stop('The data has <NA> values and na.rm=FALSE by default.')
                      else
                        (k <- nclass.FD_local(x,
                                              na.rm=na.rm)))

    if (any(is.na(x)) & na.rm == FALSE)
      stop('The data has <NA> values and na.rm=FALSE by default.')

    tmp   <- range(x,
                   na.rm=na.rm)
    start <- tmp[1] - abs(tmp[1])/100
    end   <- tmp[2] + abs(tmp[2])/100
    R     <- end - start
    h     <- R/k
  }

  # User defines 'x' and 'k'
  else {
    if (any(is.na(x)) & na.rm == FALSE)
      stop('The data has <NA> values and na.rm=FALSE by default.')

    tmp   <- range(x,
                   na.rm=na.rm)
    start <- tmp[1] - abs(tmp[1])/100
    end   <- tmp[2] + abs(tmp[2])/100
    R     <- end - start
    h     <- R/abs(k)
  }

  fdt <- make.fdt.simple(x,
                         start,
                         end,
                         h,
                         right)

  breaks <- c(start,
              end,
              h,
              ifelse (right,
                      1,
                      0))

  names(breaks) <- c('start',
                     'end',
                     'h',
                     'right')

  res <- list(table=fdt,
              breaks=breaks)

  return (res)
}

