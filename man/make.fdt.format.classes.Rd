\name{make.fdt.format.classes}
\alias{make.fdt.format.classes}

\title{
 This function formats the presentation's appearance of the interval classes
}                   

\description{
  This function uses part of the \pkg{fdt} results which contains the interval 
  classes description and format it according to a pattern wich is a regular 
  expression, the same as used by \code{\link[base]{sprintf}}. It is mainly for 
  internal use of the \pkg{fdth} package.
}

\usage{
  make.fdt.format.classes(x,
                          right,
                          pattern)
}

\arguments{
  \item{x}{A \samp{fdt} object.}
  \item{right}{Intervals right open (default = \code{FALSE}).}
  \item{pattern}{Same as \code{fmt} in \code{\link[base]{sprintf}}.}
}

\details{
  This function uses the  object[["table"]] of the \samp{fdt} results (object) 
  which contains the interval classes description and format it according to a 
  pattern wich is a regular expression, the same as used by \code{\link[base]{sprintf}}.
  It is  called by the generic function \code{summary} and is mainly for 
  internal use of the \pkg{fdth} package.
}

\value{
  The function \code{make.fdt.format.classes} returns a \code{data.frame} which 
  contains the formatted interval class values.
}

\seealso{
 \code{\link[base]{sprintf}} and \code{\link[base]{gsub}} provided by \pkg{base} package.
}

\author{
  José Cláudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
}

\note{
  This function is mainly for internal use in the \pkg{fdth} package, and may
  not remain available (unless we see a good reason).
}
