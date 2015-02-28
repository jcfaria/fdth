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
  \item{x}{A \code{fdt} (simple or multiple) object.}
  \item{i}{A vector of length up to the length of probs}
  \item{probs}{vector of probabilities defining the quantiles}
  \item{\dots}{Potencial further arguments (required by generic).}
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

\seealso{\code{median.fdt}, \code{var.fdt}.
}

\author{
  José Cláudio Faria \cr
  Enio G. Jelihovschi \cr
  Ivan B. Allaman
}

\note{
  ...
}

\examples{
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

apply(mdf,
      2,
      quantile)[2,]              # The first quartile

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

median(fdt(mdf))                 # The median (all the results are the same) ;)
}
