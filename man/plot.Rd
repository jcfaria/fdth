\name{plot.fdt}
\alias{plot.fdt}
\alias{plot.fdt.default}
\alias{plot.fdt.multiple}
\alias{plot.fdt_cat.default}
\alias{plot.fdt_cat.multiple}

\title{
  Plot fdt.default and fdt.multiple objects
}

\description{
  S3 methods for \code{fdt.default} and \code{fdt.multiple} objects.
  It is possible to plot histograms and polygons (absolute, relative
  and cumulative).
}

\usage{
## S3 methods  
\method{plot}{fdt.default}(x,
     type=c('fh', 'fp', 
            'rfh', 'rfp', 
            'rfph', 'rfpp', 
            'd',
            'cdh', 'cdp', 
            'cfh', 'cfp', 
            'cfph', 'cfpp'),
     v=FALSE,
     v.round=2,
     v.pos=3,
     xlab="Class limits",
     xlas=0,
     ylab=NULL,
     col="gray",
     xlim=NULL,
     ylim=NULL,
     main=NULL,
     x.round=2, \dots)

\method{plot}{fdt.multiple}(x,
     type=c('fh', 'fp', 
            'rfh', 'rfp', 
            'rfph', 'rfpp', 
            'd',
            'cdh', 'cdp', 
            'cfh', 'cfp', 
            'cfph', 'cfpp'),
     v=FALSE,
     v.round=2,
     v.pos=3,
     xlab="Class limits",
     xlas=0,
     ylab=NULL, 
     col="gray",
     xlim=NULL,
     ylim=NULL,
     main=NULL,
     main.vars=TRUE,
     x.round=2, \dots)

\method{plot}{fdt_cat.default}(x,
     type=c('fb', 'fp', 'fd',
            'rfb', 'rfp', 'rfd',
            'rfpb', 'rfpp', 'rfpd',
            'cfb', 'cfp', 'cfd',
            'cfpb', 'cfpp', 'cfpd',
            'pa'),
     v=FALSE,
     v.round=2,
     v.pos=3,
     xlab=NULL,
     xlas=0,
     ylab=NULL,
     y2lab=NULL,
     y2cfp=seq(0, 100, 25),
     col=gray(.4),
     xlim=NULL,
     ylim=NULL,
     main=NULL,
     box=FALSE, \dots)

\method{plot}{fdt_cat.multiple}(x,
     type=c('fb', 'fp', 'fd',
            'rfb', 'rfp', 'rfd',
            'rfpb', 'rfpp', 'rfpd',
            'cfb', 'cfp', 'cfd',
            'cfpb', 'cfpp', 'cfpd',
            'pa'),
     v=FALSE,
     v.round=2,
     v.pos=3,
     xlab=NULL,
     xlas=0,
     ylab=NULL, 
     y2lab=NULL,
     y2cfp=seq(0, 100, 25),
     col=gray(.4),
     xlim=NULL,
     ylim=NULL,
     main=NULL,
     main.vars=TRUE,
     box=FALSE, \dots)
}

\arguments{
  \item{x}{A \samp{fdt} object.}
  \item{type}{The type of the plot: \cr
    \samp{fb:} Absolute frequency barplot, \cr
    \samp{fh:} Absolute frequency histogram, \cr
    \samp{fp:} Absolute frequency polygon, \cr
    \samp{fd:} Absolute frequency dotchart, \cr

    \samp{rfb:} Relative frequency barplot, \cr
    \samp{rfh:} Relative frequency histogram, \cr
    \samp{rfp:} Relative frequency polygon, \cr
    \samp{rfd:} Relative frequency dotchart, \cr

    \samp{rfpb:} Relative frequency (\%) barplot, \cr
    \samp{rfph:} Relative frequency (\%) histogram, \cr
    \samp{rfpp:} Relative frequency (\%) polygon, \cr
    \samp{rfpd:} Relative frequency (\%) dotchart, \cr

    \samp{d:} Density, \cr
    \samp{cdh:} Cumulative density histogram, \cr
    \samp{cdp:} Cumulative density polygon, \cr

    \samp{cfb:} Cumulative frequency barplot, \cr
    \samp{cfh:} Cumulative frequency histogram, \cr
    \samp{cfp:} Cumulative frequency polygon, \cr
    \samp{cfd:} Cumulative frequency dotchart, \cr

    \samp{cdpb:} Cumulative frequency (\%) barplot, \cr
    \samp{cdph:} Cumulative frequency (\%) histogram, \cr
    \samp{cfpp:} Cumulative frequency (\%) polygon, \cr
    \samp{cfpd:} Cumulative frequency (\%) dotchart. \cr

    \samp{pa:} Pareto chart.}
  \item{v}{Logical flag: should the values be added to the plot?}
  \item{v.round}{If \code{v=TRUE}, it rounds the values to the specified number of decimal places (default 0).}
  \item{v.pos}{If \code{v=TRUE}, a position specifier for the text. Values of 1, 2, 3 and 4, respectively
    indicate positions below, to the left of, above and to the right of the coordinates (default 3).}
  \item{xlab}{A label for the \samp{x} axis.}
  \item{xlas}{An integer which controls the orientation of the \samp{x} axis labels: \cr
    \samp{0:} parallel to the axes, \cr
    \samp{2:} perpendicular to the axes.}
  \item{ylab}{A label for the \samp{y} axis.}
  \item{y2lab}{A label for the \samp{y2} axis.}
  \item{y2cfp}{A cumulative percent frequency for the \samp{y2} axis. The default is \code{seq(0, 100, 25)}.}
  \item{col}{A \code{vector} of colors.}
  \item{xlim}{The \samp{x} limits of the plot.}
  \item{ylim}{The \samp{y} limits of the plot.}
  \item{main}{Title of the plot(s). This option has priority over \samp{main.vars}, i.e, if any value is informed,
    the variable names will not be used as title of the plot(s). For \code{fdt.multiple}, the value should be a vector of characters,
    in this case, the R's recycling rule will be used.}
  \item{main.vars}{Logical flag: should the variables names be added as title of each plot (default \code{TRUE})?}
  \item{x.round}{A numeric value to round the \samp{x} ticks:
    \samp{0:} parallel to the axes, \cr
    \samp{1:} horizontal, \cr
    \samp{2:} perpendicular to the axes, \cr
    \samp{3:} vertical.}
  \item{box}{If \code{TRUE} (the default!), a box will be placed on the graphics. Only to categorical variable.}
  \item{\dots}{Optional plotting parameters.}
}

\details{
  The result is a single histogram or polygon (absolute, relative or cumulative)
  for \code{fdt.default} or a set of histograms or polygon (absolute, relative or
  cumulative) for \code{fdt.multiple} objects.
  Both \samp{default and multiple} try to compute the maximum number of histograms
  that will fit on one page, then it draws a matrix of histograms. More than one
  graphical device may be opened to show all histograms.
  
  The result is a single barplot, polygon, dotchar (absolute, relative or cumulative)
  and Pareto chart for \code{fdt_cat.default} or a set of the same graphs for 
  \code{fdt_cat.multiple} objects.
  Both \samp{default and multiple} try to compute the maximum number of histograms
  that will fit on one page, then it draws a matrix of graphs lited above. More than one
  graphical device may be opened to show all graphs.
}

\author{
  José Cláudio Faria \cr
  Enio G. Jelihovschi \cr
  Ivan B. Allaman
}

\seealso{
  \code{\link[Hmisc]{hist.data.frame}} provided by \pkg{Hmisc} package.
}

\examples{
library(fdth)

#================================
# Vectors: univariated numerical
#================================
x <- rnorm(n=1e3,
           mean=5,
           sd=1)

(d <- fdt(x))

# Histograms
plot(d)  # Absolute frequency histogram

plot(d,
     main='My title')

plot(d,
     x.round=3,
     col='darkgreen')

plot(d,
     xlas=2)

plot(d,
     x.round=3,
     xlas=2,
     xlab=NULL)

plot(d, 
     v=TRUE, 
     cex=.8,
     x.round=3,
     xlas=2,
     xlab=NULL,
     col=rainbow(11))

plot(d,
     type='fh')    # Absolute frequency histogram

plot(d,
     type='rfh')   # Relative frequency histogram

plot(d,
     type='rfph')  # Relative frequency (%) histogram

plot(d,
     type='cdh')   # Cumulative density histogram

plot(d,
     type='cfh')   # Cumulative frequency histogram

plot(d,
     type='cfph')  # Cumulative frequency (%) histogram

# Poligons
plot(d,
     type='fp')    # Absolute frequency polygon

plot(d,
     type='rfp')   # Relative frequency polygon

plot(d,
     type='rfpp')  # Relative frequency (%) polygon

plot(d,
     type='cdp')   # Cumulative density polygon

plot(d,
     type='cfp')   # Cumulative frequency polygon

plot(d,
     type='cfpp')  # Cumulative frequency (%) polygon

# Density
plot(d,
     type='d')     # Density

# Theoretical curve and fdt
x <- rnorm(1e5,
           mean=5,
           sd=1)

plot(fdt(x,
         k=100), 
     type='d', 
     col=heat.colors(100))

curve(dnorm(x,
            mean=5,
            sd=1),
      col='darkgreen',
      add=TRUE,
      lwd=2)


#==================================
# Vectors: univariated categorical
#==================================
x <- sample(letters[1:5],
            1e3,
            rep=TRUE)

(dc <- fdt_cat(x))

# Barplot: the default
plot(dc)

# Barplot
plot(dc,
     type='fb')

# Polygon
plot(dc,
     type='fp')

# Dotchart
plot(dc,
     type='fd')

# Pareto chart
plot(dc,
     type='pa')

#=============================================
# Data.frames: multivariated with categorical
#=============================================
mdf <- data.frame(X1=rep(LETTERS[1:4], 25),
                  X2=as.factor(rep(1:10, 10)),
                  Y1=c(NA, NA, rnorm(96, 10, 1), NA, NA),
                  Y2=rnorm(100, 60, 4),
                  Y3=rnorm(100, 50, 4),
                  Y4=rnorm(100, 40, 4))

# Histograms
(d <- fdt(mdf))

plot(d,
     v=TRUE,
     cex=.8)

plot(d,
     col='darkgreen',
     ylim=c(0, 40))

plot(d,
     col=rainbow(8), 
     ylim=c(0, 40),
     main=LETTERS[1:4])

plot(d,
     type='fh')

plot(d,
     type='rfh')

plot(d,
     type='rfph')

plot(d,
     type='cdh')

plot(d,
     type='cfh')

plot(d,
     type='cfph')

# Poligons
plot(d,
     v=TRUE,
     type='fp')

plot(d,
     type='rfp')

plot(d,
     type='rfpp')

plot(d,
     type='cdp')

plot(d,
     type='cfp')

plot(d,
     type='cfpp') 

# Density
plot(d,
     type='d')     

levels(mdf$X1)

plot(fdt(mdf,
         k=5,
         by='X1'),
     ylim=c(0, 12))

levels(mdf$X2)

plot(fdt(mdf,
         breaks='FD', 
         by='X2'))

plot(fdt(mdf,
         k=5,
         by='X2'))  # It is difficult to compare

plot(fdt(mdf,
         k=5,
         by='X2'),
     ylim=c(0, 8))  # Easy

plot(fdt(iris,
         k=5))

plot(fdt(iris,
         k=5),
     col=rainbow(5))

plot(fdt(iris, 
         k=5,
         by='Species'),
     v=TRUE)

d <- fdt(iris,
         k=10)

plot(d)

plot(d, 
     type='d')

# Categorical data 
(dc <- fdt_cat(mdf))
plot(dc)

plot(dc,
     type='fd',
     pch=19)

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
