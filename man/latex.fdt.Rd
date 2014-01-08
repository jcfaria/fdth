\name{latex.fdt}
\alias{latex.fdt}

\title{
  LaTeX Table of the Frequency Distribution Table
}

\description{
  This function returns a LaTeX table of the \code{fdt} objects.
}

\usage{
latex.fdt(x,
          columns=1:6,
          round=2,
          format.classes=TRUE,
          pattern='\%.2f',
          replace.breaks=TRUE,
          where='!tbp',
          caption=NULL,
          label=NULL,
          size='', 
          algtable=c('\\\\flushleft', '\\\\centering', '\\\\flushright'),
          hline1='\\\\hline',
          header=NULL,
          hline2='\\\\hline',
          algclim='l',
          algfreq='r',
          hline3='\\\\hline')
}

\arguments{
  \item{x}{A \code{fdt} object.}
  \item{columns}{A \code{vector} of \code{integers} to select columns of the
    \code{data.frame} table. For example: \code{columns=c(1:2, 4, 6)}.}
  \item{round}{Rounds the fractionary columns of the \code{fdt} to the specified number of decimal places.
   The default is 2.}
  \item{format.classes}{Logical, if \code{TRUE}, the default, the first column of the
    data.frame table will be formated using regular expression according with \samp{pattern} argument.}
  \item{pattern}{Same as \code{fmt} in \code{\link[base]{sprintf}}.
    The default is \samp{'\%.2f'}.}
  \item{replace.breaks}{Logical, if \code{TRUE}, the default, the mathematical symbols for breaks: \samp{[,)} or \samp{(,]},
   will be replaced by the LaTeX \samp{$\vdash$} or \samp{$\dashv$} symbols.}
  \item{where}{Specifies the location to which the floating body can move.
    The default is \samp{'!tbp'}.
    Possible values are the tabular environment same, e.g., \samp{'h'}, \samp{'b'} and \samp{'p'}.}
  \item{caption}{Is a legend of table.
    The default is \code{NULL}.}
  \item{label}{A text string representing a symbolic label for the table for referencing in the LaTeX \samp{\\label} and \samp{\\ref} commands.
    \samp{label} is useful in a Rnoweb document only if caption is also provided.}
  \item{size}{Specifies the font size of the table.
    The default is empty.
    The possible values are the same size used for letters in latex, e.g.,
    \samp{'\\\\scriptsize'}, \samp{'\\\\large'} and \samp{'\\\\LARGE'}.}
  \item{algtable}{Specifies the alignment of the table on page.
    The default is \samp{'\\\\flushleft'}. 
    Possible values are: \samp{'\\\\flushleft'}, \samp{'\\\\centering'} and \samp{'\\\\flushright'}.} 
  \item{hline1}{The line type of the table top.
    The default is \samp{'\\\\hline'}.}
  \item{header}{An alternative vector of strings for table header.}
  \item{hline2}{The line type of the lower table header.
    The default is \samp{'\\\\hline'}.}
  \item{algclim}{Specifies the alignment of the Class Limits.
    The default is \samp{'l'}. Possible others values are \samp{'c'} and \samp{'r'}.}
  \item{algfreq}{Specifies the alignment of the frequency columns.
    The default is \samp{'r'}. Possible others values are \samp{'c'} and \samp{'l'}.}
  \item{hline3}{The line type of the end table.
    The default is \samp{'\\\\hline'}.}
}

\details{
  The function \code{latex.fdt} was developed to make the life easier for those
  who make latex tables with the results of the \code{fdt} function.

  Some people could ask: why do not use the function \code{\link[Hmisc]{latex}} of the package \pkg{Hmisc} or
  \code{\link[xtable]{xtable}} of the \pkg{xtable}?
  Both \code{\link[Hmisc]{latex}} and \code{\link[xtable]{xtable}} functions are a bit complex and with many parameters
  for construct an adequate \code{fdt} table.
  It is not always intuitive to all users, discouraging some people to use the \code{\link[Hmisc]{latex}} or
  \code{\link[xtable]{xtable}} function for constructing fdt tables, making the preparation of the fdt analysis
  reports data laborious. 

  The function \code{latex.fdt} is extremely easy to use.
  Obviously, the function provides a formatting pre-set according to what the authors think ideal
  for presentation in articles, reports, among others.
  If the user is not satisfied with the formatting provided by this function, the functions \code{\link[Hmisc]{latex}}
  and \code{\link[xtable]{xtable}} provides arguments that allow formatting of tables according to user need.

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

  The available parameters offer an easy and powerful way to format the \samp{fdt}
  for publications and other purposes.
}

\value{
  An object of the class \code{latex.fdt}.
}

\author{
  José Cláudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  and\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
}

\seealso{
  \code{\link{fdt}},
  \code{\link[Hmisc]{latex}},
  \code{\link[xtable]{xtable}}
}  

\examples{

library(fdth)

##Example 1: The simplest possible
t1 <- fdt(rnorm(n=1e3,
                mean=10,
                sd=2))

t1x <- latex.fdt(t1)

t1x


## Example 2
(t1x <- latex.fdt(t1,
                  replace.breaks=FALSE,
                  columns=c(1:2, 4, 6)))


## Example 3
t2 <- fdt(rnorm(n=1e3,
                mean=10,
                sd=2),
          right=TRUE)

t2x <- latex.fdt(t2,
                 algtable='\\\\centering',
                 caption='Frequency distribution table 2',
                 label='tbl-2',
                 pattern='\%.1f')

t2x


## Example 4
t3 <- fdt(rnorm(n=1e3,
                mean=10,
                sd=2))

t3x <- latex.fdt(t3,
                 algtable='\\\\flushright',
                 caption='Frequency distribution table 3',
                 label='tbl-3',
                 pattern='\%.1e')

t3x
}

\keyword{multivariate, table, latex, fdt}

