\name{ta}
\alias{ta}

\title{
  Shortcut for total amplitude
}

\description{
  \code{ta} is a shortcut to \code{amplitude}.
}

\usage{
ta(x, \dots)
}

\arguments{
  \item{x}{an object accepted by \code{amplitude}.}
  \item{\dots}{additional arguments passed to \code{amplitude}.}
}

\details{
  This function is equivalent to calling \code{amplitude(x, \dots)}.
}

\value{
  Returns the same output as \code{amplitude}.
}

\seealso{
  \code{amplitude}.
}

\examples{
library(fdth)

x <- rnorm(1e3,
           20,
           2)

ta(x)
ta(fdt(x))
ta(fdt(iris[, 1:4]))
}

\keyword{fdt}
\keyword{range}
