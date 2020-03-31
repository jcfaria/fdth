\name{xtable.fdt}
\alias{xtable.fdt}
\alias{xtable.fdt.multiple}
\alias{xtable.fdt_cat.multiple} 

\title{
  LaTeX table of the frequency distribution table
}

\description{
  This function returns a LaTeX table of the \code{fdt}, \code{fdt.multiple} and \code{fdt_cat.multiple} objects of the \code{xtable} class.
}

\usage{
xtable.fdt(x,
           caption = NULL,
           label = NULL,
           align = NULL,
           digits = NULL,
           display = NULL,
           auto = FALSE,
           \dots) 

xtable.fdt.multiple(x,
                    caption = NULL,
                    label = NULL,
                    align = NULL,
                    digits = NULL,
                    display = NULL,
                    \dots) 

xtable.fdt_cat.multiple(x,
                        caption = NULL,
                        label = NULL,
                        align = NULL,
                        digits = NULL,
                        display = NULL,
                        \dots) 
}

\arguments{
  \item{x}{A \code{fdt} or \code{fdt.multiple} class object.}
  \item{caption}{Character vector of length 1 or 2 containing the table's caption or title. See \code{xtable} function to more details.}
  \item{label}{Character vector of length 1 containing the LaTeX label or HTML anchor. See \code{xtable} function to more details.}
  \item{align}{Character vector of length equal to the number of columns of the resulting table, indicating the alignment of the corresponding columns. See \code{xtable} function to more details.}
  \item{digits}{Numeric vector of length equal to one (in which case it will be replicated as necessary) or to the number of columns of the resulting table or matrix of the same size as the resulting table, indicating the number of digits to display in the corresponding columns. See \code{xtable} function to more details.}
  \item{display}{Character vector of length equal to the number of columns of the resulting table, indicating the format for the corresponding columns. See \code{xtable} function to more details.}
  \item{auto}{Logical, indicating whether to apply automatic format when no value is passed to \code{align}, \code{digits}, or \code{display}}
  \item{\dots}{Additional arguments.}
}

\details{
  The functions \code{latex.fdt} was deprecated. We understand over the years that creating a method for the generic \code{xtable} function would be inevitable, given the advancement of the xtable package and its support by the academic community.

  Then, the \code{fdt}, \code{fdt.multiple} and \code{fdt_cat.multiple} methods were created for the generic \code{xtable} function. For object of the \code{fdt_cat} class, no methods were created, as they inherit the \code{data.frame} class, and therefore, the \code{xtable} functions can be used directly for such objects.

  Objects of the \code{fdt.multiple} and \code{fdt_cat.multiple} class, when using the \code{xtable} function, will have the \code{xtableList} class. Although it may seem confusing, the \code{xtableList} function in the xtable package has no generic function and therefore it was not possible to create a method of type \code{xtableList.fdt.multiple}. Therefore, a method of the \code{xtable.fdt.multiple} class was created, but the function \code{xtableList} is being used internally.

  More examples than those provided in the manual can be seen in the vignette.

  It is possible to select what columns of the table (a \code{data.frame}) will be shown, as well as the pattern of the first column. The columns are:
  \enumerate{
    \item \samp{Class limits} 
    \item \samp{f} - Absolute frequency 
    \item \samp{rf} - Relative frequency 
    \item \samp{rf(\%)} - Relative frequency, \% 
    \item \samp{cf} - Cumulative frequency 
    \item \samp{cf(\%)} - Cumulative frequency, \% 
  }
}

\value{
  An object of the class \code{xtable.fdt} and \code{xtable.fdt.multiple}.
}

\author{
  José Cláudio Faria \cr
  Enio G. Jelihovschi \cr
  Ivan B. Allaman
}

\seealso{
  \code{\link[xtable]{xtable}},
  \code{\link[xtable]{xtableList}}
}  

\examples{

library(fdth)
library(xtable)

# +++++ Quantitative data

##Example 1: The simplest possible
t1 <- fdt(rnorm(n=1e3,
                mean=10,
                sd=2))

t1x <- xtable(t1)
t1x

## Example 2
print(t1x,
      include.rownames=FALSE)

## Example 3
newclass <- gsub("[\\\\[\\\\)]","",t1x[,1],perl=TRUE)
t3x <- t1x
t3x[,1] <- newclass

print(t3x,
      include.rownames=FALSE,
      sanitize.text.function = function(x)gsub(",",
                                               "\\\\dashv",
                                               x,
                                               perl = TRUE))

## Example 4
# clim <- t1$table[1]
# clim1 <- sapply(clim,
#                 as.character)
# right <- t1$breaks[4]
# pattern <- "\\%05.2f"
# clim2 <- make.fdt.format.classes(clim1,
#                                  right,
#                                  pattern)
# t4x <- t1x
# t4x[,1] <- clim2
# 
# print(t4x,
#       include.rownames=FALSE)
# 
## Example 5
t5 <- fdt(iris,
          by="Species")
attr(t5, "subheadings") <- paste0("Variable = ",
                      names(t5))
xtable(t5)

# +++++ Qualitative data

## Example 6
t6 <- fdt_cat(sample(LETTERS[1:3], 
                     replace=TRUE,
                     size=30))

t6x <- xtable(t6)
t6x

t61 <- fdt_cat(data.frame(c1=sample(LETTERS[1:3],
                                    replace=TRUE,
                                    size=10),
                          c2=sample(letters[4:5],
                                    replace=TRUE,
                                    size=10)))
attr(t61, "subheadings") <- paste0("Variable = ",
                       names(t61)) 
t61x <- xtable(t61)
t61x
}

\keyword{multivariate, xable, fdt}
