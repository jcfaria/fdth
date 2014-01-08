\name{fdth-package}
\alias{fdth-package}
\alias{fdth}
\docType{package}

\title{
  Frequency Distribution Tables, Histograms and Polygons
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

  Frequency distribution table \samp{fdt} can be used for both ordinal and
  continuous variables. 

  The \code{R} environment provides a set of functions (generally low level)
  enabling the user to perform a \samp{fdt} and the associated graphical representation,
  the histogram. A \samp{fdt} plays an important role to summarize data information and 
  is the basis for the estimation of probability density function used in 
  parametrical inference.

  However, for novices or ocasional users of \code{R}, it can be laborious to
  find out all necessary functions and graphical parameters to do a normalized 
  and pretty \samp{fdt} and the associated histogram ready for publications.
  
  That is the aim of this package, i.e, to allow the user to do (using a few,
  simple and flexible high level set of S3 functions) with ease and flexibility
  both: the \samp{fdt} and histogram. The input data for univariated is generally
  a \code{vector}. For multivariated data can be used both: a \code{data.frame},
  in this case also allowing grouping all numerical variables according to one
  categorical, or \code{matrices}.

  The simplest way to run \samp{fdt} is done by supplying only the \samp{x}
  object, for example: \code{d <- fdt(x)}. In this case all necessary
  default values (\samp{breaks} and \samp{right}) ("Sturges" and \code{FALSE}
  respectively) will be used.

  It can be provided also:
  \itemize{
    \item \samp{x} and \samp{k} (number of class intervals);
    \item \samp{x}, \samp{start} (left endpoint of the first class interval) and
    \samp{end} (right endpoint of the last class interval); or
    \item \samp{x}, \samp{start}, \samp{end} and \samp{h} (class interval width).
  }
  These options make the \samp{fdt} very easy and flexible.

  The \samp{fdt} object stores information to be used by methods \code{summary},
  \code{print} and \code{plot}. The result of \code{plot} is a histogram or
  polygon (absolute, relative or cumulative).
  The methods \code{summary}, \code{print} and \code{plot} provide a reasonable
  set of parameters to format and plot the \samp{fdt} object in a pretty
  (and publishable) way.
}

\author{
  José Cláudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
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
set.seed(1)

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
     x.las=2)

plot(d,
     x.round=3,
     x.las=2,
     xlab=NULL)

plot(d,
     v=TRUE,
     cex=.8,
     x.round=3,
     x.las=2,
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

# Polygons
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

# Summary
d

summary(d)  # the same

print(d)    # the same

show(d)     # the same

summary(d,
        format=TRUE)      # It can not be what you want to publications!

summary(d,
        format=TRUE,
        pattern='\%.2f')   # Huumm ..., good, but ... Can it be better?

summary(d,
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
d[['table']]                        # Stores the feq. dist. table (fdt)
d[['breaks']]                       # Stores the breaks of fdt
d[['breaks']]['start']              # Stores the left value of the first class
d[['breaks']]['end']                # Stores the right value of the last class
d[['breaks']]['h']                  # Stores the class interval
as.logical(d[['breaks']]['right'])  # Stores the right option

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

# Histograms
plot(d,
     v=TRUE)

plot(d,
     col='darkgreen')

plot(d,
     col=rainbow(8))

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

# Summary
d

summary(d)  # the same

print(d)    # the same

show(d)     # the same

summary(d,
        format=TRUE)

summary(d,
        format=TRUE, 
        pattern='\%05.2f')  # regular expression

summary(d,
        col=c(1:2, 4, 6), 
        format=TRUE,
        pattern='\%05.2f')

print(d,
      col=c(1:2, 4, 6))

print(d,
      col=c(1:2, 4, 6), 
      format=TRUE,
      pattern='\%05.2f')

# Using by
levels(mdf$X1)

summary(fdt(mdf,
            k=5,
            by='X1'))

plot(fdt(mdf,
         k=5,
         by='X1'),
     col=rainbow(5))

levels(mdf$X2)

summary(fdt(mdf,
            breaks='FD',
            by='X2'), 
        round=3)

plot(fdt(mdf,
         breaks='FD',
         by='X2'))

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
