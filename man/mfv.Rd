% ***Enio: necessário intervenção geral e mais detalhadamente onde deixei reticências: ...***
\name{mfv}
\alias{mfv}
\alias{mfv.default}
\alias{mfv.fdt}
\alias{mfv.fdt.multiple}
\alias{mfv.fdt.cat}
\alias{mfv.fdt.cat.multiple}

\title{
  Most frequenty value (statistical mode) of frequency distribution table (numerical and categorical variable)
}

\description{
  S3 methods for the most frequenty value (statistical mode) of a \code{fdt}. \cr 
  Useful to estimate the most frequenty value or statistical mode (when the real data vector is not known) from a previous \code{fdt}.
}

\usage{
## S3 generic
mfv(x, \dots)

## S3 methods: numerical and categorical 
\method{mfv}{default}(x, \dots)

\method{mfv}{fdt}(x, \dots)

\method{mfv}{fdt.multiple}(x, \dots)

\method{mfv}{fdt.cat}(x, \dots)

\method{mfv}{fdt.cat.multiple}(x, \dots)
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


## Categorical
mdf <- data.frame(c1=sample(letters[1:5],
                            1e3,
                            rep=TRUE),
                  c2=sample(letters[6:10],
                            1e3,
                            rep=TRUE),
                  c3=sample(letters[11:21],
                            1e3,
                            rep=TRUE))
head(mdf)

mfv(mdf$c1)   # From vector c1
mfv(mdf$c2)   # From vector c2
mfv(mdf$c3)   # From vector c3

(tb <- fdt.cat(mdf))

mfv(tb)      # From agruped dad in a fdt 
}   
