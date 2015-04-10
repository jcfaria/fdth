% DONE
\name{summary.latex.fdt}
\alias{summary.latex.fdt}

\title{
  Summary method for latex.fdt and latex.fdt_cat objects
}

\description{
  Returns a summary list for \code{latex.fdt} and \code{latex.fdt_cat} objects.
}

\usage{
## S3 method
\method{summary}{latex.fdt}(object, \dots)
}

\arguments{
  \item{object}{A given object of the class \code{\link{latex.fdt}} or \code{\link{latex.fdt_cat}}.}
  \item{\dots}{Potential further arguments (require by generic).}
}

\author{
  José Cláudio Faria \cr
  Enio G. Jelihovschi \cr
  Ivan B. Allaman
}

\seealso{
  \code{\link{fdt}},
}

\examples{
library(fdth)

##Example 1: The simplest possible
t1 <- fdt(rnorm(n=1e3,
                mean=10,
                sd=2))

t1x <- latex.fdt(t1)

summary(t1x)


## Example 2
(t1x <- latex.fdt(t1,
                  replace.breaks=FALSE,
                  columns=c(1:2, 4, 6)))

## Example 3
t2 <- fdt(rnorm(n=1e3,
                mean=10,
                sd=2),
          right=TRUE)

t2x <- latex.fdt(t2,
                 algtable='\\\\centering',
                 caption='Frequency distribution table 2',
                 label='tbl-2',
                 pattern='\%.1f')

summary(t2x)

## Example 4
t3 <- fdt(rnorm(n=1e3,
                mean=10,
                sd=2))

t3x <- latex.fdt(t3,
                 algtable='\\\\flushright',
                 caption='Frequency distribution table 3',
                 label='tbl-3',
                 pattern='\%.1e')

summary(t3x)
}

\keyword{fdt, latex.fdta, summary}
