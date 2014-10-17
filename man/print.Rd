% ***Enio: adaptar para contemplar também as categóricas***
\name{print.fdt}
\alias{print.fdt}
\alias{print.fdt.default}
\alias{print.fdt.multiple}
\alias{print.fdt.cat.default}
\alias{print.fdt.cat.multiple}

\title{
  Print methods for fdt objects
}

\description{
  S3 methods to return a \code{data.frame} (the frequency
  distribution table - fdt) for \code{fdt.default} and \code{fdt.multiple}
  objects.
}

\usage{
## S3 methods
\method{print}{fdt.default}(x,
      columns=1:6,
      round=2,
      format.classes=FALSE,
      pattern='\%09.3e',
      row.names=FALSE,
      right=TRUE, \dots)

\method{print}{fdt.multiple}(x,
      columns=1:6,
      round=2,
      format.classes=FALSE,
      pattern='\%09.3e',
      row.names=FALSE,
      right=TRUE, \dots)

\method{print}{fdt.cat.default}(x,
      columns=1:6,
      round=2,
      row.names=FALSE,
      right=TRUE, \dots)

\method{print}{fdt.cat.multiple}(x,
      columns=1:6,
      round=2,
      row.names=FALSE,
      right=TRUE, \dots)
}

\arguments{
  \item{x}{A \samp{fdt} object.}
  \item{columns}{A \code{vector} of \code{integers} to select columns of the
    \code{data.frame} table.}
  \item{round}{Rounds the fractionary columns of \samp{fdt} to the specified number of
    decimal places (default 2).}
  \item{format.classes}{Logical, if \code{TRUE} the first column of the
    data.frame table will be formated using regular expression.
    The default is \dQuote{\%09.3e}.}
  \item{pattern}{Same as \code{fmt} in \code{\link[base]{sprintf}}.}
  \item{row.names}{Logical (or character vector), indicating whether (or what)
    row names should be printed. The default is \code{FALSE}.}
  \item{right}{Logical, indicating whether or not strings should be right-aligned.
    The default is right-alignment.}
  \item{\dots}{Potential further arguments (require by generic).}
}

\details{
  It is possible to select what columns of the table (a \code{data.frame})
  will be shown, as well as the pattern of the first column. The columns are:
  \enumerate{
    \item \samp{Class limits} 
    \item \samp{f} - Absolute frequency 
    \item \samp{rf} - Relative frequency 
    \item \samp{rf(\%)} - Relative frequency, \% 
    \item \samp{cf} - Cumulative frequency 
    \item \samp{cf(\%)} - Cumulative frequency, \% 
  }

  The available parameters offer an easy and powerful way to format the \samp{fdt} for
  publications and other purposes.
}

\value{
  A single \code{data.frame} for \code{fdt.default} or multiple
  \code{data.frames} for \code{fdt.multiple}.
}

\author{
  José Cláudio Faria \cr
  Enio G. Jelihovschi
}

\examples{
library (fdth)

#======================
# Vectors: univariated
#======================
set.seed(1)

x <- rnorm(n=1e3,
           mean=5,
           sd=1)

d <- fdt(x)

str(d)

d

print(d) # the same

print(d,
      format=TRUE)      # It can not be what you want to publications!

print(d,
      format=TRUE,
      pattern='\%.2f')   # Huumm ..., good, but ... Can it be better?

print(d,
      col=c(1:2, 4, 6),
      format=TRUE, 
      pattern='\%.2f')   # Yes, it can!

range(x)                # To know x

print(fdt(x,
          start=1,
          end=9,
          h=1),
      col=c(1:2, 4, 6),
      format=TRUE,
      pattern='\%d')     # Is it nice now?

d[['table']]                        # Stores the feq. dist. table (fdt)
d[['breaks']]                       # Stores the breaks of fdt
d[['breaks']]['start']              # Stores the left value of the first class
d[['breaks']]['end']                # Stores the right value of the last class
d[['breaks']]['h']                  # Stores the class interval
as.logical(d[['breaks']]['right'])  # Stores the right option

#=============================================
# Data.frames: multivariated with categorical
#=============================================
mdf <- data.frame(X1=rep(LETTERS[1:4], 25),
                  X2=as.factor(rep(1:10, 10)),
                  Y1=c(NA, NA, rnorm(96, 10, 1), NA, NA),
                  Y2=rnorm(100, 60, 4),
                  Y3=rnorm(100, 50, 4),
                  Y4=rnorm(100, 40, 4))

(d <- fdt(mdf))

str(d)

print(d) # the same

print(d,
      format=TRUE)

print(d,
      format=TRUE,
      pattern='\%05.2f')  # regular expression

print(d,
      col=c(1:2, 4, 6), 
      format=TRUE, 
      pattern='\%05.2f')

print(d,
      col=c(1:2, 4, 6))

print(d,
      col=c(1:2, 4, 6),
      format=TRUE,
      pattern='\%05.2f')

levels(mdf$X1)

print(fdt(mdf,
          k=5,
          by='X1'))

levels(mdf$X2)

print(fdt(mdf,
          breaks='FD',
          by='X2'),
      round=3)

print(fdt(mdf,
          k=5,
          by='X2'),
      format=TRUE,
      round=3)

print(fdt(iris,
          k=5),
      format=TRUE,
      patter='\%04.2f')

levels(iris$Species)

print(fdt(iris,
          k=5,
          by='Species'),
      format=TRUE, 
      patter='\%04.2f')

#=========================
# Matrices: multivariated
#=========================
print(fdt(state.x77), 
      col=c(1:2, 4, 6),
      format=TRUE)

print(fdt(volcano,
          right=TRUE),
      col=c(1:2, 4, 6),
      round=3,
      format=TRUE,
      pattern='\%05.1f')
}

\keyword{fdt}
\keyword{frequency}
\keyword{distribution}
\keyword{table}
\keyword{print}
