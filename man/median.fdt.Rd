\name{median.fdt}
\alias{median.fdt}
\alias{median.fdt.multiple}

\title{
  Median of frequency distribution table (numerical variable)
}

\description{
  S3 method for the median of a \code{fdt}. \cr 
  Useful to estimate the median (when the real data vector is not known) from a previous \code{fdt}.
}

\usage{
## S3 method: numerical 
\method{median}{fdt}(x, \dots)
}

\arguments{
  \item{x}{a \code{fdt} (simple or multiple) object.}
  \item{\dots}{required by generic.}
}

\details{\code{median.fdt} calculates the median value based on a known formula.
\code{median.fdt.multiple} calls \code{median.fdt} for each variable, that is, each column of the data.frame.
}

\value{\code{median.fdt} returns a numeric vector containing the value of the median of the \code{fdt}.
 \code{median.fdt.multiple} returns a list, where each element is a numeric vector 
 containing the value of the median of the \code{fdt} for each variable.
}

\seealso{\code{mean.fdt}, \code{mfv}.
}

\author{
  Faria, J. C. \cr
  Allaman, I. B \cr
  Jelihovschi, E. G.
}

\examples{
library(fdth)

mdf <- data.frame(x=rnorm(1e3,
                          20, 
                          2),
                  y=rnorm(1e3, 
                          30,
                          3),
                  z=rnorm(1e3,
                          40,
                          4))

head(mdf)

# From a data.frame
apply(mdf,
      2,
      median)

# From a fdt object
median(fdt(mdf))
}
