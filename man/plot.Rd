\name{plot.fdt}
\alias{plot.fdt}
\alias{plot.fdt.default}
\alias{plot.fdt.multiple}

\title{
  Plot fdt.default and fdt.multiple objects}

\description{
  S3 methods for \code{fdt.default} and \code{fdt.multiple} objects.
  It is possible to plot histograms and poligons (absolute, relative
  and cumulative).}

\usage{
\method{plot}{fdt.default}(x, type=c('fh', 'fp', 'rfh', 'rfp', 'rfph', 'rfpp',
  'd', 'cdh', 'cdp', 'cfh', 'cfp', 'cfph', 'cfpp'),
   v=FALSE, v.round=2, v.pos=3,
   xlab="Class limits", ylab=NULL, col="gray", xlim=NULL, ylim=NULL,
   main=NULL, x.round=2, x.las=1, \dots)

\method{plot}{fdt.multiple}(x, type=c('fh', 'fp', 'rfh', 'rfp', 'rfph', 'rfpp',
  'd', 'cdh', 'cdp', 'cfh', 'cfp', 'cfph', 'cfpp'),
  v=FALSE, v.round=2, v.pos=3,
  xlab="Class limits", ylab=NULL, col="gray", xlim=NULL, ylim=NULL,
  main=NULL, main.vars=TRUE, x.round=2, x.las=1, \dots)}

\arguments{
  \item{x}{A \code{fdt} object.}
  \item{type}{The type of the plot: \cr
    \code{fh:} Absolut frequency histogram, \cr
    \code{fp:} Absolut frequency poligon, \cr
    \code{rfh:} Relative frequency histogram, \cr
    \code{rfp:} Relative frequency poligon, \cr
    \code{rfph:} Relative frequency (\%) histogram, \cr
    \code{rfpp:} Relative frequency (\%) poligon, \cr
    \code{d:} Density, \cr
    \code{cdh:} Cumulative density histogram, \cr
    \code{cdp:} Cumulative density poligon, \cr
    \code{cfh:} Cumulative frequency histogram, \cr
    \code{cfp:} Cumulative frequency poligon, \cr
    \code{cdph} Cumulative frequency (\%) histogram, \cr
    \code{cfpp:} Cumulative frequency (\%) poligon.}
  \item{v}{Logical flag: should the values be added to the plot?}
  \item{v.round}{If \code{v=TRUE}, it rounds the values to the specified number of decimal places (default 0).}
  \item{v.pos}{If \code{v=TRUE}, a position specifier for the text. Values of `1', `2', `3' and `4', respectively
    indicate positions below, to the left of, above and to the right of the coordinates (default 3).}
  \item{xlab}{A label for the x axis.}
  \item{ylab}{A label for the y axis.}
  \item{col}{A \code{vector} of colors.}
  \item{xlim}{The x limits of the plot.}
  \item{ylim}{The y limits of the plot.}
  \item{main}{Title of the plot(s). This option has priority over \code{main.vars}, i.e, if any value is informed,
   the variable names will not be used as title of the plot(s). For \code{fdt.multiple}, the value should be a vector of characters,
   in this case, the R's recycling rule will be used.}
  \item{main.vars}{Logical flag: should the variables names be added as title of each plot (default TRUE)?}
  \item{x.round}{A numeric value to round the x ticks.}
  \item{x.las}{An integer which controls the orientation of the x axis labels
    (0: parallel to the axes, 1: horizontal, 2: perpendicular to the axes,
    3: vertical)}
  \item{\dots}{Optional plotting parameters.}}

\details{
  The result is a single histogram or poligon (absolute, relative or cummulative)
  for \code{fdt.default} or a set of histograms or poligon (absolute, relative or
  cummulative) for \code{fdt.multiple} objects.
  Both \code{default and multiple} try to compute the maximum number of histograms
  that will fit on one page, then it draws a matrix of histograms. More than one
  graphical device may be opened to show all histograms.}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr}

\seealso{
  \code{\link[Hmisc]{hist.data.frame}} provided by \code{Hmisc} package.}

\examples{
library(fdth)

#======================
# Vectors: univariated
#======================
set.seed(1)
x <- rnorm(n=1e3, mean=5, sd=1)
d <- fdt(x); d

# Histograms
plot(d)                # Absolut frequency histogram
plot(d, main='My title')
plot(d, x.round=3, col='darkgreen')
plot(d, x.las=2)
plot(d, x.round=3, x.las=2, xlab=NULL)
plot(d, v=TRUE, cex=.8, x.round=3, x.las=2, xlab=NULL, col=rainbow(11))

plot(d, type='fh')     # Absolut frequency histogram
plot(d, type='rfh')    # Relative frequency histogram
plot(d, type='rfph')   # Relative frequency (%) histogram
plot(d, type='cdh')    # Cumulative density histogram
plot(d, type='cfh')    # Cumulative frequency histogram
plot(d, type='cfph')   # Cumulative frequency (%) histogram

# Poligons
plot(d, type='fp')     # Absolut frequency poligon
plot(d, type='rfp')    # Relative frequency poligon
plot(d, type='rfpp')   # Relative frequency (%) poligon
plot(d, type='cdp')    # Cumulative density poligon
plot(d, type='cfp')    # Cumulative frequency poligon
plot(d, type='cfpp')   # Cumulative frequency (%) poligon

# Density
plot(d, type='d')      # Density

# Theoretical curve and fdt
x <- rnorm(1e5, mean=5, sd=1)
plot(fdt(x, k=100), type='d', col=heat.colors(100))
curve(dnorm(x, mean=5, sd=1), col='darkgreen', add=TRUE, lwd=2)

#=============================================
# Data.frames: multivariated with categorical
#=============================================
mdf <- data.frame(X1 = rep(LETTERS[1:4], 25),
                  X2 = as.factor(rep(1:10, 10)),
                  Y1 = c(NA, NA, rnorm(96, 10, 1), NA, NA),
                  Y2 = rnorm(100, 60, 4),
                  Y3 = rnorm(100, 50, 4),
                  Y4 = rnorm(100, 40, 4))

# Histograms
d <- fdt(mdf); d
plot(d, v=TRUE, cex=.8)
plot(d, col='darkgreen', ylim=c(0, 40))
plot(d, col=rainbow(8), ylim=c(0, 40), main=LETTERS[1:4])

plot(d, type='fh')
plot(d, type='rfh')
plot(d, type='rfph')
plot(d, type='cdh')
plot(d, type='cfh')
plot(d, type='cfph')

# Poligons
plot(d, v=TRUE, type='fp')
plot(d, type='rfp')
plot(d, type='rfpp')
plot(d, type='cdp')
plot(d, type='cfp')
plot(d, type='cfpp') 

# Density
plot(d, type='d')     

levels(mdf$X1)
plot(fdt(mdf, k=5, by='X1'), ylim=c(0, 12))

levels(mdf$X2)
plot(fdt(mdf, breaks='FD', by='X2'))

plot(fdt(mdf, k=5, by='X2'))               # It is dificult to compare
plot(fdt(mdf, k=5, by='X2'), ylim=c(0, 8)) # Easy

plot(fdt(iris, k=5))
plot(fdt(iris, k=5), col=rainbow(5))

plot(fdt(iris, k=5, by='Species'), v=TRUE)

d <- fdt(iris, k=10)
plot(d)
plot(d, type='d')

#=========================
# Matrices: multivariated
#=========================
plot(fdt(state.x77))

plot(fdt(volcano))
}

\keyword{fdt}
\keyword{frequency}
\keyword{distribution}
\keyword{table}
\keyword{plot}
\keyword{histogram}
