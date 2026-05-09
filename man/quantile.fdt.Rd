\name{quantile.fdt}
\alias{quantile.fdt}
\alias{quantile.fdt.multiple}

\title{
  Quantile of frequency distribution table (numerical variable)
}

\description{
  S3 methods for the quantile of a \code{fdt}. \cr 
  Useful to estimate the quantile (when the real data vector is not known) from a previous \code{fdt}.
}

\usage{
## S3 methods: numerical
\method{quantile}{fdt}(x,
         \dots,
         i=1,
         probs=seq(0, 1, 0.25))

\method{quantile}{fdt.multiple}(x, \dots) 
}

\arguments{
  \item{x}{a \code{fdt} (simple or multiple) object.}
  \item{i}{an integer vector indicating which quantiles should be computed.
  Values must be in \code{1:(length(probs)-1)}.}
  \item{probs}{a finite numeric vector in \code{[0, 1]} defining the quantile
  scale. It must have at least two values and be sorted in non-decreasing order.}
  \item{\dots}{potential further arguments (required by generic).}
}

\details{\code{quantile.fdt} calculates the quantiles based on a known formula for 
 class intervals. \code{quantile.fdt.multiple} calls \code{quantile.fdt}
 for each variable, that is, each column of the data.frame.
}

\value{\code{quantile.fdt} returns a numeric vector containing the value(s) of the 
quantile(s) from \code{fdt}.
 \code{quantile.fdt.multiple} returns a list, where each element is a numeric vector 
 containing the quantile(s) of the \code{fdt} for each variable.
}

\seealso{\code{median.fdt}, \code{\link{fdt_var}}, \code{\link{mfv}}.
}

\author{
  Faria, J. C. \cr
  Allaman, I. B \cr
  Jelihovschi, E. G.
}

\examples{
x <- rnorm(n=1e3,
           mean=5,
           sd=1)

(ft <- fdt(x))

# Quartile from vector
quantile(x)[2:4]

# Quartile from grouped data in a fdt object
quantile(ft,
         i=1:3)

# Quartile from vector
quantile(x,
         probs=seq(from=0,
                   to=1,
                   by=.1))[2:10]

# Decile from grouped data in a fdt object
quantile(ft,
         i=1:9,
         probs=seq(from=0,
                   to=1,
                   by=.1))

# Percentile from grouped data in a fdt object
quantile(ft,
         i=1:99,
         probs=seq(from=0,
                   to=1,
                   by=.01))

# From a data.frame
mdf <- data.frame(x=rnorm(1e2, 
                          20, 
                          2),
                  y=rnorm(1e2, 
                          30,
                          3),
                  z=rnorm(1e2,
                          40,
                          4))

head(mdf)

# From a data.frame (rows 2:4 are the 25\%, 50\%, and 75\% quantiles)
apply(mdf,
      2,
      quantile)[2:4, ]

# From a fdt object
quantile(fdt(mdf))               # Notice that the i default is 1 (the first quartile)

## A small (but didactic) joke  
quantile(fdt(mdf),
         i=2,
         probs=seq(0, 
                   1, 
                   0.25))        # The quartile 2
quantile(fdt(mdf),
         i=5,
         probs=seq(0, 
                   1, 
                   0.10))        # The decile 5 

quantile(fdt(mdf),
         i=50,
         probs=seq(0, 
                   1, 
                   0.01))        # The percentile 50

quantile(fdt(mdf),
         i=500,
         probs=seq(0, 
                   1, 
                   0.001))       # The permile 500

median(fdt(mdf))                 # The median (all results are the same) ;)

# More than one quantile
quantile(fdt(mdf$x),
         i=1:3,
         probs=seq(0,
                   1,
                   0.25))  # The three quartiles

quantile(fdt(mdf$x),
         i=1:9,
         probs=seq(0,
                   1,
                   0.10))  # The nine deciles

# Legacy approach (no longer necessary)
# ql <- numeric()
# 
# for(i in 1:3)
#   ql[i] <- quantile(fdt(mdf$x),
#                     i=i,
#                     probs=seq(0,
#                               1,
#                               0.25))  # The three quartiles
# 
# names(ql) <- paste0(c(25,
#                       50,
#                       75),
#                     '\%')
# round(ql,
#       2)
}
