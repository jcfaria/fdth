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

  The simplest way to run \samp{fdt} and \samp{fdt_cat} is by supplying only the \samp{x}
  object, for example: \code{d <- fdt(x)}. In this case all necessary
  default values (\samp{breaks} and \samp{right}) ("Sturges" and \code{FALSE}
  respectively) will be used, if the \samp{x} object is categorical then just use
  \code{d <- fdt_cat(x)}.

  If the varable is of contiuos type, you can also supply:
  \itemize{
    \item \samp{x} and \samp{k} (number of class intervals);
    \item \samp{x}, \samp{start} (left endpoint of the first class interval) and
    \samp{end} (right endpoint of the last class interval); or
    \item \samp{x}, \samp{start}, \samp{end} and \samp{h} (class interval width).
  }
  These options make the \samp{fdt} very easy and flexible.

  The \samp{fdt} and \samp{fdt_cat} object store information to be used by methods \code{summary},
  \code{print} and \code{plot}. The result of \code{plot} is a histogram or
  polygon (absolute, relative or cumulative).
  The methods \code{summary}, \code{print} and \code{plot} provide a reasonable
  set of parameters to format and plot the \samp{fdt} object in a pretty
  (and publishable) way.
}

\author{
  Faria, J. C. \cr
  Allaman, I. B \cr
  Jelihovschi, E. G.
}

\keyword{fdt}
\keyword{frequency}
\keyword{distribution}
\keyword{table}
\keyword{histogram}

\seealso{
  \code{\link[graphics]{hist}} provided by \pkg{graphics} and
  \code{\link[base]{table}}, \code{\link[base]{cut}} both provided by \pkg{base}.
}

\examples{
library (fdth)

# Numerical
#======================
# Vectors: univariated
#======================
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

# Polygons
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

# Summary
ft

summary(ft)  # the same

print(ft)    # the same

show(ft)     # the same

summary(ft,
        format=TRUE)      # It can not be what you want to publications!

summary(ft,
        format=TRUE,
        pattern='\%.2f')   # Huumm ..., good, but ... Can it be better?

summary(ft,
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
ft[['table']]                        # Stores the feq. dist. table (fdt)
ft[['breaks']]                       # Stores the breaks of fdt
ft[['breaks']]['start']              # Stores the left value of the first class
ft[['breaks']]['end']                # Stores the right value of the last class
ft[['breaks']]['h']                  # Stores the class interval
as.logical(ft[['breaks']]['right'])  # Stores the right option

# Theoretical curve and fdt
y <- rnorm(1e5,
           mean=5, 
           sd=1)

ft <- fdt(y,
          k=100)

plot(ft,
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
                  Y4=rnorm(100, 40, 4),
                  stringsAsFactors=TRUE)

#(ft <- fdt(mdf)) # Error message due to presence of NA values

(ft <- fdt(mdf,
           na.rm=TRUE))

# Histograms
plot(ft,
     v=TRUE)

plot(ft,
     col=rainbow(8))

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

# Summary
ft

summary(ft)  # the same

print(ft)    # the same

show(ft)     # the same

summary(ft,
        format=TRUE)

summary(ft,
        format=TRUE, 
        pattern='\%05.2f')  # regular expression

summary(ft,
        col=c(1:2, 4, 6), 
        format=TRUE,
        pattern='\%05.2f')

print(ft,
      col=c(1:2, 4, 6))

print(ft,
      col=c(1:2, 4, 6), 
      format=TRUE,
      pattern='\%05.2f')

# Using by
levels(mdf$X1)

plot(fdt(mdf,
         k=5,
         by='X1',
         na.rm=TRUE),
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

## Categorical
x <- sample(x=letters[1:5],
            size=5e2,
            rep=TRUE)

(fdt.c <- fdt_cat(x))

(fdt.c <- fdt_cat(x,
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

(fdt.c <- fdt_cat(mdf))

(fdt.c <- fdt_cat(mdf,
                  dec=FALSE))

(fdt.c <- fdt_cat(mdf,
                  sort=FALSE))

(fdt.c <- fdt_cat(mdf,
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

(fdt.c <- fdt_cat(x))

}
