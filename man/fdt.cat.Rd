% ***Enio: eu adaptei da fdt, necessário correção***
\name{fdt.cat}
\alias{fdt.cat}
\alias{fdt.cat.default}
\alias{fdt.cat.data.frame}
\alias{fdt.cat.matrix}

\title{
  Frequency distribution table for categorical data.
}

\description{
  A S3 set of methods to easily perform categorical frequency distribution table (\samp{fdt.cat}) from
  \code{vector}, \code{data.frame} and \code{matrix} objects.
}

\usage{
## S3 generic
fdt.cat(x, \dots)

## S3 methods
\method{fdt.cat}{default}(x,
        sort=TRUE,
        decreasing=TRUE, \dots)

\method{fdt.cat}{data.frame}(x,
        by,
        sort=TRUE,
        decreasing=TRUE, \dots)

\method{fdt.cat}{matrix}(x,
        sort=TRUE,
        decreasing=TRUE, \dots)
}

\arguments{
  \item{x}{A \code{vector}, \code{data.frame} or \code{matrix} object.
    If \samp{x} is \code{data.frame} or \code{matrix} it must contain at least 
    one character/factor column.}
  \item{by}{Categorical variable used for grouping each categorical response,
    useful only on \code{data.frame}.} 
  \item{sort}{Logical. Should the \code{fdt.cat} be sorted by the absolute frequency
    into ascending or descending order? (default = \code{TRUE}).}
  \item{decreasing}{Logical. Should the sort order be increasing or decreasing?
    (default = \code{TRUE}).}
  % TODO: Ao invés de "required by generic" dizer para quais funcões os argumentos adicionais são repassados.
  \item{\dots}{Optional further arguments (required by generic).}
}

\details{
  The simplest way to run \samp{fdt.cat} is supplying only the \samp{x}
  object, for example: \code{ct <- fdt.cat(x)}. In this case all necessary
  default values (\samp{sort = TRUE} and \samp{decreasing = TRUE}) will be used.

  These options make the \samp{fdt.cat} very easy and flexible.

  The \samp{fdt.cat} object stores information to be used by methods \code{summary},
  \code{print}, \code{plot} and \code{mfv}. The result of plot is ....
  The methods \code{summary.fdt.cat}, \code{print.fdt.cat} and \code{plot.fdt.cat} provide a reasonable
  set of parameters to format and plot the \samp{fdt.cat} object in a pretty
  (and publishable) way.
}

\value{
  For \code{fdt.cat} the method \code{fdt.cat.default} returns a \code{data.frame} storing the \samp{fdt}.

  The methods \code{fdt.cat.data.frame} and \code{fdt.cat.matrix} 
  return a list of class \code{fdt.cat..multiple}.
  This \code{list} has one slot for each categorical
  variable of the  supplied \samp{x}. Each slot, corresponding to each categorical
  variable, stores the same slots of the \code{fdt.cat.default} described above.
}

\author{
  José Cláudio Faria \cr
  Enio G. Jelihovschi
}

\seealso{
  \code{\link[graphics]{hist}} provided by \pkg{graphics};
  \code{\link[base]{table}}, \code{\link[base]{cut}} both provided by \pkg{base}
  and \code{\link[Hmisc]{hist.data.frame}} provided by \pkg{Hmisc} package.
}

\examples{
library(fdth)

## Categorical
x <- sample(x=letters[1:5],
            size=5e2,
            rep=TRUE)

(fdt.c <- fdt.cat(x))

(fdt.c <- fdt.cat(x,
                  sort=FALSE))

##================================================
## Data.frame: multivariated with two categorical
##================================================
mdf <- data.frame(c1=sample(LETTERS[1:3], 1e2, rep=TRUE),
                  c2=as.factor(sample(1:10, 1e2, rep=TRUE)),
                  n1=c(NA, NA, rnorm(96, 10, 1), NA, NA),
                  n2=rnorm(100, 60, 4),
                  n3=rnorm(100, 50, 4))

head(mdf)

(fdt.c <- fdt.cat(mdf))

(fdt.c <- fdt.cat(mdf,
                  dec=FALSE))

(fdt.c <- fdt.cat(mdf,
                  sort=FALSE))

##================================================
## Matrix: two categorical
##================================================
x <- matrix(sample(x=letters[1:10],
                   size=100,
                   rep=TRUE),
            nc=2,
            dimnames=list(NULL,
                          c('c1', 'c2')))

head(x)

(fdt.c <- fdt.cat(x))
}

\keyword{fdt}
\keyword{fdt.cat}
\keyword{frequency}
\keyword{distribution}
\keyword{table}
