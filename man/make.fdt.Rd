% ***Enio: necessário intervenção onde deixei reticências: ...***
\name{make.fdt}
\alias{make.fdt}
\alias{make.fdt.cat}

\title{
  Frequency distribution table for continuous and categorical variables
}

\description{
  Makes a full fdt from a minimal set of information. \cr
  Useful to reproduce (when the real data vector is not known) a previous fdt.
}

\usage{
make.fdt(f,
         start,
         end,
         right=FALSE)

make.fdt.cat(f,
             categories=NULL,
             sort=TRUE,
             decreasing=TRUE)
}

\arguments{
  \item{f}{A numeric \code{vector} object of frequency.}
  \item{start}{The left value of the interval of the first class.}
  \item{end}{The last value of the interval of the last class.}
  \item{right}{Intervals right open (default = \code{FALSE}).}
  \item{categories}{...}
  \item{sort}{...}
  \item{decreasing}{...}
}

\details{
  ...
}

\value{
  ...
}

\seealso{
  ...
}

\author{
  José Cláudio Faria \cr
  Enio G. Jelihovschi
}

\note{
  ...
}

\examples{
## Numeric
## Making one reference fdt
set.seed(33)
x <- rnorm(1e3, 
           20, 
           2)

(tb.r <- fdt(x))

## Making a brand new
(tb.n  <- make.fdt(f=tb.r$table$f,
                   start=13.711,
                   end=27.229))   # Huumm ..., good, but ... Can it be better? 

summary(tb.n,
        format=TRUE,
        pattern='\%.3f')           # Is it nice now?


## Categorical
x <- sample(letters[1:5], 
            1e3, 
            rep=TRUE)

## Making one reference fdt
(tb.r <- fdt.cat(x))

## Making a brand new
(tb.n <- make.fdt.cat(f=tb.r$f,
                      categ=tb.r$Category))
}
