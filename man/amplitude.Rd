\name{amplitude}
\alias{amplitude}
\alias{amplitude.default}
\alias{amplitude.fdt}
\alias{amplitude.fdt.multiple}

\title{
  Total range from frequency distribution table classes
}

\description{
  S3 methods for the total range (amplitude total) of an object.
  For \code{fdt}, it is computed as \code{end - start} from class limits.
}

\usage{
## S3 generic
amplitude(x, \dots)

## S3 methods: numerical
\method{amplitude}{default}(x, \dots)

\method{amplitude}{fdt}(x, \dots)

\method{amplitude}{fdt.multiple}(x, \dots)
}

\arguments{
  \item{x}{a numeric vector or a \code{fdt} (simple or multiple) object.}
  \item{\dots}{additional arguments passed to methods.}
}

\details{
  \code{amplitude.default} computes \code{max(x) - min(x)}.
  \code{amplitude.fdt} computes \code{breaks["end"] - breaks["start"]}.
  \code{amplitude.fdt.multiple} applies \code{amplitude.fdt} to each variable.
}

\value{
  \code{amplitude.default} and \code{amplitude.fdt} return a numeric value.
  \code{amplitude.fdt.multiple} returns a list of numeric values, one per variable.
}

\seealso{
  \code{var.fdt}, \code{sd.fdt}, \code{fdt}.
}

\author{
  Faria, J. C. \cr
  Allaman, I. B \cr
  Jelihovschi, E. G.
}

\examples{
library(fdth)

x <- rnorm(1e3,
           mean = 20,
           sd = 2)

# From a numeric vector
amplitude(x)

# From an fdt object
amplitude(fdt(x))

# From a data.frame (fdt.multiple)
amplitude(fdt(iris[, 1:4]))
}

\keyword{fdt}
\keyword{range}
