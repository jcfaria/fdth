% ***Enio: necessário intervenção geral e mais detalhadamente onde deixei reticências: ...***
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
  \item{x}{A \code{fdt} (simple or multiple) object.}
  \item{\dots}{Required by generic.}
}

\details{
  ...
}

\value{
  ...
}

\seealso{
  ...
}

\author{
  José Cláudio Faria \cr
  Enio G. Jelihovschi
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
      median)

median(fdt(mdf))      
}
