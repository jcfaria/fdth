\name{fdth-package}
\alias{fdth}
\alias{fdth-package}
\docType{package}

\title{
  Frequency distribution tables, histograms and polygons
}

\description{
  The \pkg{fdth} package contains a set of functions which easily allows
  the user to make frequency distribution tables (\samp{fdt}), its associated
  histograms and frequency polygons (absolute, relative and cumulative).
  The \samp{fdt} can be formatted in many ways which may be suited to
  publication in many different ways (papers, books, etc).
  The \code{plot} method (S3) is the histogram which can be dealt with the
  easiness and flexibility of a high level function.
}

\details{
  The frequency of a particular observation is the number of times the
  observation occurs in the data. The distribution of a variable is the pattern
  of frequencies of the observation.

  Frequency distribution table \samp{fdt} can be used for ordinal, continuous and  categorical variables. 

  The \code{R} environment provides a set of functions (generally low level)
  enabling the user to perform a \samp{fdt} and the associated graphical representation,
  the histogram. A \samp{fdt} plays an important role to summarize data information and 
  is the basis for the estimation of probability density function used in 
  parametrical inference.

  However, for novices or ocasional users of \code{R}, it can be laborious to
  find out all necessary functions and graphical parameters to do a normalized 
  and pretty \samp{fdt} and the associated histogram ready for publications.

  That is the aim of this package, i.e, to allow the user easily and flexibly to do
  both: the \samp{fdt} and the histogram. The most common input data for univariated is
  a \code{vector}. For multivariated data can be used both: a \code{data.frame},
  in this case also allowing grouping all numerical variables according to one
  categorical, or \code{matrices}.

  The simplest way to run \samp{fdt} and \samp{fdt.cat} is by supplying only the \samp{x}
  object, for example: \code{d <- fdt(x)}. In this case all necessary
  default values (\samp{breaks} and \samp{right}) ("Sturges" and \code{FALSE}
  respectively) will be used, if the \samp{x} object is categorical then just use
  \code{d <- fdt.cat(x)}.

  If the varable is of contiuos type, you can also supply:
  \itemize{
    \item \samp{x} and \samp{k} (number of class intervals);
    \item \samp{x}, \samp{start} (left endpoint of the first class interval) and
    \samp{end} (right endpoint of the last class interval); or
    \item \samp{x}, \samp{start}, \samp{end} and \samp{h} (class interval width).
  }
  These options make the \samp{fdt} very easy and flexible.

  The \samp{fdt} and \samp{fdt.cat} object store information to be used by methods \code{summary},
  \code{print} and \code{plot}. The result of \code{plot} is a histogram or
  polygon (absolute, relative or cumulative).
  The methods \code{summary}, \code{print} and \code{plot} provide a reasonable
  set of parameters to format and plot the \samp{fdt} object in a pretty
  (and publishable) way.
}

\author{
  José Cláudio Faria \cr
  Enio G. Jelihovschi \cr
  Ivan B. Allaman
}

\keyword{fdt}
\keyword{frequency}
\keyword{distribution}
\keyword{table}
\keyword{histogram}

\seealso{
  \code{\link[graphics]{hist}} provided by \pkg{graphics};
  \code{\link[base]{table}}, \code{\link[base]{cut}} both provided by \pkg{base}
  and
  \code{\link[Hmisc]{hist.data.frame}} provided by \pkg{Hmisc} package.
}

\examples{
library (fdth)

#======================
# Vectors: univariated
#======================
x <- rnorm(n=1e3,
           mean=5,
           sd=1)

(tb <- fdt(x))

# Histograms
plot(tb)  # Absolute frequency histogram

plot(tb,
     main='My title')

plot(tb,
     x.round=3,
     col='darkgreen')

plot(tb,
     xlas=2)

plot(tb,
     x.round=3,
     xlas=2,
     xlab=NULL)

plot(tb,
     v=TRUE,
     cex=.8,
     x.round=3,
     xlas=2,
     xlab=NULL,
     col=rainbow(11))

plot(tb,
     type='fh')    # Absolute frequency histogram

plot(tb,
     type='rfh')   # Relative frequency histogram

plot(tb,
     type='rfph')  # Relative frequency (%) histogram

plot(tb,
     type='cdh')   # Cumulative density histogram

plot(tb,
     type='cfh')   # Cumulative frequency histogram

plot(tb,
     type='cfph')  # Cumulative frequency (%) histogram

# Polygons
plot(tb,
     type='fp')    # Absolute frequency polygon

plot(tb,
     type='rfp')   # Relative frequency polygon

plot(tb,
     type='rfpp')  # Relative frequency (%) polygon

plot(tb,
     type='cdp')   # Cumulative density polygon

plot(tb,
     type='cfp')   # Cumulative frequency polygon

plot(tb,
     type='cfpp')  # Cumulative frequency (%) polygon

# Density
plot(tb,
     type='d')     # Density

# Summary
tb

summary(tb)  # the same

print(tb)    # the same

show(tb)     # the same

summary(tb,
        format=TRUE)      # It can not be what you want to publications!

summary(tb,
        format=TRUE,
        pattern='\%.2f')   # Huumm ..., good, but ... Can it be better?

summary(tb,
        col=c(1:2, 4, 6),
        format=TRUE,
        pattern='\%.2f')   # Yes, it can!

range(x)                  # To know x

summary(fdt(x,
            start=1, 
            end=9,
            h=1),
        col=c(1:2, 4, 6),
        format=TRUE,
        pattern='\%d')     # Is it nice now?

# The fdt.object
tb[['table']]                        # Stores the feq. dist. table (fdt)
tb[['breaks']]                       # Stores the breaks of fdt
tb[['breaks']]['start']              # Stores the left value of the first class
tb[['breaks']]['end']                # Stores the right value of the last class
tb[['breaks']]['h']                  # Stores the class interval
as.logical(tb[['breaks']]['right'])  # Stores the right option

# Theoretical curve and fdt
y <- rnorm(1e5,
           mean=5, 
           sd=1)

tb <- fdt(y,
          k=100)

plot(tb,
     type='d',                       # density
     col=heat.colors(100))

curve(dnorm(x,
            mean=5, 
            sd=1),
      n=1e3,      
      add=TRUE, 
      lwd=4)

#=============================================
# Data.frames: multivariated with categorical
#=============================================
mdf <- data.frame(X1=rep(LETTERS[1:4], 25),
                  X2=as.factor(rep(1:10, 10)),
                  Y1=c(NA, NA, rnorm(96, 10, 1), NA, NA),
                  Y2=rnorm(100, 60, 4),
                  Y3=rnorm(100, 50, 4),
                  Y4=rnorm(100, 40, 4))

(tb <- fdt(mdf))

# Histograms
plot(tb,
     v=TRUE)

plot(tb,
     col=rainbow(8))

plot(tb,
     type='fh')

plot(tb,
     type='rfh')

plot(tb,
     type='rfph')

plot(tb, 
     type='cdh')

plot(tb,
     type='cfh')

plot(tb,
     type='cfph')

# Poligons
plot(tb,
     v=TRUE,
     type='fp')

plot(tb,
     type='rfp')

plot(tb,
     type='rfpp')

plot(tb,
     type='cdp')

plot(tb,
     type='cfp')

plot(tb,
     type='cfpp') 

# Density
plot(tb,
     type='d') 

# Summary
tb

summary(tb)  # the same

print(tb)    # the same

show(tb)     # the same

summary(tb,
        format=TRUE)

summary(tb,
        format=TRUE, 
        pattern='\%05.2f')  # regular expression

summary(tb,
        col=c(1:2, 4, 6), 
        format=TRUE,
        pattern='\%05.2f')

print(tb,
      col=c(1:2, 4, 6))

print(tb,
      col=c(1:2, 4, 6), 
      format=TRUE,
      pattern='\%05.2f')

# Using by
levels(mdf$X1)

plot(fdt(mdf,
         k=5,
         by='X1'),
     col=rainbow(5))

levels(mdf$X2)

summary(fdt(iris,
            k=5),
        format=TRUE,
        patter='\%04.2f')

plot(fdt(iris,
         k=5),
     col=rainbow(5))

levels(iris$Species)

summary(fdt(iris,
            k=5,
            by='Species'),
        format=TRUE,
        patter='\%04.2f')

plot(fdt(iris,
         k=5,
         by='Species'),
     v=TRUE)

#=========================
# Matrices: multivariated
#=========================
summary(fdt(state.x77),
        col=c(1:2, 4, 6),
        format=TRUE)

plot(fdt(state.x77))

# Very big
summary(fdt(volcano,
            right=TRUE),
        col=c(1:2, 4, 6),
        round=3,
        format=TRUE,
        pattern='\%05.1f')

plot(fdt(volcano,
         right=TRUE))
}
