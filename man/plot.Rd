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
     x.round=2, 
     grouped=FALSE,
     args.legend=NULL, \dots)

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
  \item{type}{the type of the plot: \cr
    \samp{fb:} absolute frequency barplot, \cr
    \samp{fh:} absolute frequency histogram, \cr
    \samp{fp:} absolute frequency polygon, \cr
    \samp{fd:} absolute frequency dotchart, \cr

    \samp{rfb:} relative frequency barplot, \cr
    \samp{rfh:} relative frequency histogram, \cr
    \samp{rfp:} relative frequency polygon, \cr
    \samp{rfd:} relative frequency dotchart, \cr

    \samp{rfpb:} relative frequency (\%) barplot, \cr
    \samp{rfph:} relative frequency (\%) histogram, \cr
    \samp{rfpp:} relative frequency (\%) polygon, \cr
    \samp{rfpd:} relative frequency (\%) dotchart, \cr

    \samp{d:} density, \cr
    \samp{cdh:} cumulative density histogram, \cr
    \samp{cdp:} cumulative density polygon, \cr

    \samp{cfb:} cumulative frequency barplot, \cr
    \samp{cfh:} cumulative frequency histogram, \cr
    \samp{cfp:} cumulative frequency polygon, \cr
    \samp{cfd:} cumulative frequency dotchart, \cr

    \samp{cdpb:} cumulative frequency (\%) barplot, \cr
    \samp{cdph:} cumulative frequency (\%) histogram, \cr
    \samp{cfpp:} cumulative frequency (\%) polygon, \cr
    \samp{cfpd:} cumulative frequency (\%) dotchart. \cr

    \samp{pa:} pareto chart.}
  \item{v}{logical flag: should the values be added to the plot?}
  \item{v.round}{if \code{v=TRUE}, it rounds the values to the specified number of decimal places (default 0).}
  \item{v.pos}{if \code{v=TRUE}, a position specifier for the text. Values of 1, 2, 3 and 4, respectively
    indicate positions below, to the left of, above and to the right of the coordinates (default 3).}
  \item{xlab}{a label for the \samp{x} axis.}
  \item{xlas}{an integer which controls the orientation of the \samp{x} axis labels: \cr
    \samp{0:} parallel to the axes, \cr
    \samp{2:} perpendicular to the axes.}
  \item{ylab}{a label for the \samp{y} axis.}
  \item{y2lab}{a label for the \samp{y2} axis.}
  \item{y2cfp}{a cumulative percent frequency for the \samp{y2} axis. The default is \code{seq(0, 100, 25)}.}
  \item{col}{a \code{vector} of colors.}
  \item{xlim}{the \samp{x} limits of the plot.}
  \item{ylim}{the \samp{y} limits of the plot.}
  \item{main}{title of the plot(s). This option has priority over \samp{main.vars}, i.e, if any value is informed,
    the variable names will not be used as title of the plot(s). For \code{fdt.multiple}, the value should be a vector of characters,
    in this case, the R's recycling rule will be used.}
  \item{main.vars}{logical flag: should the variables names be added as title of each plot (default \code{TRUE})?}
  \item{x.round}{a numeric value to round the \samp{x} ticks:
    \samp{0:} parallel to the axes, \cr
    \samp{1:} horizontal, \cr
    \samp{2:} perpendicular to the axes, \cr
    \samp{3:} vertical.}
  \item{box}{if \code{TRUE} (the default!), a box will be placed on the graphics. Only to categorical variable.}
  \item{grouped}{if \code{TRUE}, the lines of the polygon types will be plotted in the same graphics. It's work only if the "by" argument is used.}
  \item{args.legend}{list of additional arguments to be passed to \code{legend}; names of the list are used as argument names. Only used if \code{grouped=TRUE}. The default is \code{NULL}.}
  \item{\dots}{optional plotting parameters.}
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
  Faria, J. C. \cr
  Allaman, I. B \cr
  Jelihovschi, E. G.
}

\examples{
library(fdth)

#================================
# Vectors: univariated numerical
#================================
x <- rnorm(n=1e3,
           mean=5,
           sd=1)

(ft <- fdt(x))

# Histograms
plot(ft)  # Absolute frequency histogram

plot(ft,
     main='My title')

plot(ft,
     x.round=3,
     col='darkgreen')

plot(ft,
     xlas=2)

plot(ft,
     x.round=3,
     xlas=2,
     xlab=NULL)

plot(ft,
     v=TRUE, 
     cex=.8,
     x.round=3,
     xlas=2,
     xlab=NULL,
     col=rainbow(11))

plot(ft,
     type='fh')    # Absolute frequency histogram

plot(ft,
     type='rfh')   # Relative frequency histogram

plot(ft,
     type='rfph')  # Relative frequency (%) histogram

plot(ft,
     type='cdh')   # Cumulative density histogram

plot(ft,
     type='cfh')   # Cumulative frequency histogram

plot(ft,
     type='cfph')  # Cumulative frequency (%) histogram

# Poligons
plot(ft,
     type='fp')    # Absolute frequency polygon

plot(ft,
     type='rfp')   # Relative frequency polygon

plot(ft,
     type='rfpp')  # Relative frequency (%) polygon

plot(ft,
     type='cdp')   # Cumulative density polygon

plot(ft,
     type='cfp')   # Cumulative frequency polygon

plot(ft,
     type='cfpp')  # Cumulative frequency (%) polygon

# Density
plot(ft,
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
      lwd=3)


#==================================
# Vectors: univariated categorical
#==================================
x <- sample(letters[1:5],
            1e3,
            rep=TRUE)

(ft.c <- fdt_cat(x))

# Barplot: the default
plot(ft.c)

# Barplot
plot(ft.c,
     type='fb')

# Polygon
plot(ft.c,
     type='fp')

# Dotchart
plot(ft.c,
     type='fd')

# Pareto chart
plot(ft.c,
     type='pa')

#=============================================
# Data.frames: multivariated with categorical
#=============================================
mdf <- data.frame(X1=rep(LETTERS[1:4], 25),
                  X2=as.factor(rep(1:10, 10)),
                  Y1=c(NA, NA, rnorm(96, 10, 1), NA, NA),
                  Y2=rnorm(100, 60, 4),
                  Y3=rnorm(100, 50, 4),
                  Y4=rnorm(100, 40, 4),
                  stringsAsFactors=TRUE)

str(mdf)

# Histograms
(ft <- fdt(mdf,
           na.rm=TRUE))

plot(ft,
     v=TRUE,
     cex=.8)

plot(ft,
     col='darkgreen',
     ylim=c(0, 40))

plot(ft,
     col=rainbow(8), 
     ylim=c(0, 40),
     main=LETTERS[1:4])

plot(ft,
     type='fh')

plot(ft,
     type='rfh')

plot(ft,
     type='rfph')

plot(ft,
     type='cdh')

plot(ft,
     type='cfh')

plot(ft,
     type='cfph')

# Poligons
plot(ft,
     v=TRUE,
     type='fp')

plot(ft,
     type='rfp')

plot(ft,
     type='rfpp')

plot(ft,
     type='cdp')

plot(ft,
     type='cfp')

plot(ft,
     type='cfpp') 

# Density
plot(ft,
     type='d')     

levels(mdf$X1)

plot(fdt(mdf,
         k=5,
         by='X1',
         na.rm=TRUE),
     ylim=c(0, 12))

levels(mdf$X2)

plot(fdt(mdf,
         breaks='FD', 
         by='X2',
         na.rm=TRUE))

plot(fdt(mdf,
         k=5,
         by='X2',
         na.rm=TRUE))  # It is difficult to compare

plot(fdt(mdf,
         k=5,
         by='X2',
         na.rm=TRUE),
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

ft <- fdt(iris,
          k=10)

plot(ft)

plot(ft,
     type='d')

# Categorical data 
(ft.c <- fdt_cat(mdf))
plot(ft.c)

plot(ft.c,
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
