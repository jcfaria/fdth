% ***Enio: necessário intervenção geral e mais detalhadamente onde deixei reticências: ...***
\name{sd}
\alias{sd}
\alias{sd.default}
\alias{sd.fdt}
\alias{sd.fdt.multiple}

\title{
  Standart deviation of frequency distribution table (numerical variable)
}

\description{
  S3 methods for the standart deviation of a \code{fdt}. \cr 
  Useful to estimate the standart deviation (when the real data vector is not known) from a previous \code{fdt}.
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
      sd)

sd(fdt(mdf))      
}
