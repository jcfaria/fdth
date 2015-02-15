% ***Enio: necessário intervenção geral e mais detalhadamente onde deixei reticências: ...***
\name{mfv}
\alias{mfv}
\alias{mfv.default}
\alias{mfv.fdt}
\alias{mfv.fdt.multiple}
%\alias{mfv.fdt.cat}
%\alias{mfv.fdt.cat.multiple}

\title{
  Most frequent value (statistical mode) of frequency distribution table (numerical variable)
}

\description{
  S3 methods for the most frequent value (statistical mode) of a \code{fdt}. \cr 
  Useful to estimate the most frequent value or statistical mode (when the real data vector is not known) from a previous \code{fdt}.
}

\usage{
## S3 generic
mfv(x, \dots)

## S3 methods: numerical and categorical 
\method{mfv}{default}(x, \dots)

\method{mfv}{fdt}(x, \dots)

\method{mfv}{fdt.multiple}(x, \dots)

#\method{mfv}{fdt.cat}(x, \dots)

#\method{mfv}{fdt.cat.multiple}(x, \dots)
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
## Numerial
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

mfv(mdf$x)   # From vector x
mfv(mdf$y)   # From vector y
mfv(mdf$z)   # From vector z

(tb <- fdt(mdf))

mfv(tb)      # From agruped dad in a fdt
}   
