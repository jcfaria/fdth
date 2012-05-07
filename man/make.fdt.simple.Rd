\name{make.fdt.simple}
\alias{make.fdt.simple}

\title{
  A function which makes the table with absolute and relative frequencies, 
  cumulative frequencies in numbers and percentages.}

\description{
  This function is called by \code{fdt.default} and \code{make.fdt.multiple}. It makes a table 
  from a vector of numbers. The table consists of absolute and relative 
  frequencies, cumulative frequencies in numbers and percentages.}

\usage{
  make.fdt.simple(x, start, end, h, right)}

\arguments{
  \item{x}{A numeric \code{vector} object.}
  \item{start}{The left value of the interval of the first class.}
  \item{end}{The last value of the interval of the last class.}
  \item{h}{The class interval.}
  \item{right}{Intervals right open (default = FALSE).}}

\details{
  This function is called by \code{fdt.default} and makes a table from a vector of
  numbers. The table consists of absolute and relative frequencies, cumulative 
  frequencies in numbers and percentages. The result is used by the generic 
  function \code{summary} to format it in a table suited for publication.}

\value{
  The function \code{make.fdt.simple} returns a \code{data.frame} which contains the 
  table with interval classes and frequencies. The following are the columns:
  \item{Class limits}{Interval classes, \code{character}}
  \item{f}{Absolute frequency, \code{numeric}}
  \item{rf}{Relative frequency, \code{numeric}}
  \item{rf(\%)}{Relative frequency in percentages, \code{numeric}}
  \item{cf}{Cumulative frequency; \code{numeric}}
  \item{cf(\%)}{Cumulative frequency in percentages, \code{numeric}}}

\seealso{
 \code{\link[base]{table}} and \code{\link[base]{cut}} provided by \code{base} package.}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr}

\note{
  This function is mainly for internal use in the \code{fdt} package, and may
  not remain available (unless we see a good reason).}
