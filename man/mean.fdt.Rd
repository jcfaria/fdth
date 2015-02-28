\name{mean.fdt}
\alias{mean.fdt}
\alias{mean.fdt.multiple}

\title{
  Mean of frequency distribution table (numerical variable)
}

\description{
  S3 method for the arithmetic mean of a \code{fdt}. \cr 
  Useful to estimate the arithmetic mean (when the real data vector is not known) from a previous \code{fdt}.
}

\usage{
## S3 method: numerical 
\method{mean}{fdt}(x, \dots)
}

\arguments{
  \item{x}{A \code{fdt} (simple or multiple) object.}
  \item{\dots}{Required by generic.}
}

\details{\code{mean.fdt} calculates the mean value based on a known formula using 
the midpoint of each interval class. \code{mean.fdt.multiple} calls \code{mean.fdt}
 for each variable, that is, each column of the data.frame.
}

\value{\code{mean.fdt} returns a numeric vector containing the mean value of the \code{fdt}.
 \code{mean.fdt.multiple} returns a list, where each element is a numeric vector 
 containing the mean value of the \code{fdt} for each variable.
}

\seealso{\code{median.fdt}, \code{mfv}.
}

\author{
  José Cláudio Faria \cr
  Enio G. Jelihovschi \cr
  Ivan B. Allaman
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
      mean)

mean(fdt(mdf))      
}
