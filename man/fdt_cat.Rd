\name{fdt_cat}
\alias{fdt_cat}
\alias{fdt_cat.default}
\alias{fdt_cat.data.frame}
\alias{fdt_cat.matrix}

\title{
  Frequency distribution table for categorical data
}

\description{
  A S3 set of methods to easily perform categorical frequency distribution table (\samp{fdt_cat}) from
  \code{vector}, \code{data.frame} and \code{matrix} objects.
}

\usage{
## S3 generic
fdt_cat(x, \dots)

## S3 methods
\method{fdt_cat}{default}(x,
        sort=TRUE,
        decreasing=TRUE, \dots)

\method{fdt_cat}{data.frame}(x,
        by,
        sort=TRUE,
        decreasing=TRUE, \dots) 

\method{fdt_cat}{matrix}(x,
        sort=TRUE,
        decreasing=TRUE, \dots)
}

\arguments{
  \item{x}{a \code{vector}, \code{data.frame} or \code{matrix} object.
    If \samp{x} is \code{data.frame} or \code{matrix} it must contain at least 
    one character/factor column.}
   \item{by}{categorical variable used for grouping each categorical response,
    useful only on \code{data.frame}.}  
  \item{sort}{logical. Should the \code{fdt_cat} be sorted by the absolute frequency
    into ascending or descending order? (default = \code{TRUE}).}
  \item{decreasing}{logical. Should the sort order be increasing or decreasing?
    (default = \code{TRUE}).}
  \item{\dots}{optional further arguments (required by generic).}
}

\details{
  The simplest way to run \samp{fdt_cat} is supplying only the \samp{x}
  object, for example: \code{ct <- fdt_cat(x)}. In this case all necessary
  default values (\samp{sort = TRUE} and \samp{decreasing = TRUE}) will be used.

  These options make the \samp{fdt_cat} very easy and flexible.

  The \samp{fdt_cat} object stores information to be used by methods \code{summary},
  \code{print}, \code{plot} and \code{mfv}. The result of plot is a bar plot.
  The methods \code{summary.fdt_cat}, \code{print.fdt_cat} and \code{plot.fdt_cat} provide a reasonable
  set of parameters to format and plot the \samp{fdt_cat} object in a pretty
  (and publishable) way.
}

\value{
  For \code{fdt_cat} the method \code{fdt_cat.default} returns a \code{data.frame} storing the \samp{fdt}.

  The methods \code{fdt_cat.data.frame} and \code{fdt_cat.matrix} 
  return a list of class \code{fdt_cat..multiple}.
  This \code{list} has one slot for each categorical
  variable of the  supplied \samp{x}. Each slot, corresponding to each categorical
  variable, stores the same slots of the \code{fdt_cat.default} described above.
}

\author{
  José Cláudio Faria \cr
  Enio G. Jelihovschi\cr
  Ivan B. Allaman
}

\seealso{
  \code{\link[graphics]{hist}} provided by \pkg{graphics} and
  \code{\link[base]{table}}, \code{\link[base]{cut}} both provided by \pkg{base}.
}

\examples{
library(fdth)

# Categorical
x <- sample(x=letters[1:5],
            size=5e2,
            rep=TRUE)

table(x)

(ft.c <- fdt_cat(x))

(ft.c <- fdt_cat(x,
                 sort=FALSE))

#================================================
# Data.frame: multivariated with two categorical
#================================================
mdf <- data.frame(c1=sample(LETTERS[1:3], 1e2, rep=TRUE),
                  c2=as.factor(sample(1:10, 1e2, rep=TRUE)),
                  n1=c(NA, NA, rnorm(96, 10, 1), NA, NA),
                  n2=rnorm(100, 60, 4),
                  n3=rnorm(100, 50, 4),
                  stringsAsFactors=TRUE)

head(mdf)

(ft.c <- fdt_cat(mdf))

(ft.c <- fdt_cat(mdf,
                 dec=FALSE))

(ft.c <- fdt_cat(mdf,
                 sort=FALSE))

(ft.c <- fdt_cat(mdf,
                 by='c1'))

#================================================
# Matrix: two categorical
#================================================
x <- matrix(sample(x=letters[1:10],
                   size=100,
                   rep=TRUE),
            nc=2,
            dimnames=list(NULL,
                          c('c1', 'c2')))

head(x)

(ft.c <- fdt_cat(x))
}

\keyword{fdt}
\keyword{fdt_cat}
\keyword{frequency}
\keyword{distribution}
\keyword{table}
