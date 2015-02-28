\name{sd}
\alias{sd}
\alias{sd.default}
\alias{sd.fdt}
\alias{sd.fdt.multiple}

\title{
  Standard deviation of frequency distribution table (numerical variable)
}

\description{
  S3 methods for the standard deviation of a \code{fdt}. \cr 
  Useful to estimate the standard deviation (when the real data vector is not known) from a previous \code{fdt}.
}

\usage{
## S3 generic
sd(x, \dots)

## S3 methods: numerical
\method{sd}{default}(x, \dots)

\method{sd}{fdt}(x, \dots)

\method{sd}{fdt.multiple}(x, \dots)
}

\arguments{
  \item{x}{A \code{fdt} (simple or multiple) object.}
  \item{\dots}{Required to be generic.}
}

\details{\code{sd.fdt} calculates the value of the variance based on a known formula. 
\code{sd.fdt.multiple} calls \code{sd.fdt}for each variable, that is, each column of the data.frame.
}

\value{\code{sd.fdt} returns a numeric vector containing the value of the median of the \code{fdt}.
 \code{median.fdt.multiple} returns a list, where each element is a numeric vector 
 containing the value of the variance of the \code{fdt} for each variable.
}

\seealso{\code{var.fdt}, \code{mean.fdt}.
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

apply(mdf,
      2,
      sd)

sd(fdt(mdf))      
}
