\name{var}
\alias{var}
\alias{var.default}
\alias{var.fdt}
\alias{var.fdt.multiple}

\title{
  Variance of frequency distribution table (numerical variable)
}

\description{
  S3 methods for the variance of a \code{fdt}. \cr 
  Useful to estimate the variance (when the real data vector is not known) from a previous \code{fdt}.
}

\usage{
## S3 generic
var(x, \dots)

## S3 methods: numerical
\method{var}{default}(x, \dots)

\method{var}{fdt}(x, \dots)

\method{var}{fdt.multiple}(x, \dots)
}

\arguments{
  \item{x}{A \code{fdt} (simple or multiple) object.}
  \item{\dots}{Required to be generic.}
}

\details{\code{var.fdt} calculates the value of the variance based on a known formula. 
\code{var.fdt.multiple} calls \code{var.fdt}for each variable, that is, each column of the data.frame.
}

\value{\code{var.fdt} returns a numeric vector containing the value of the median of the \code{fdt}.
 \code{median.fdt.multiple} returns a list, where each element is a numeric vector 
 containing the value of the variance of the \code{fdt} for each variable.
}

\seealso{\code{sd.fdt}, \code{mean.fdt}.
}

\author{
  José Cláudio Faria \cr
  Enio G. Jelihovschi \cr
  Ivan B. Allaman
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
      var)

var(fdt(mdf))      
}
