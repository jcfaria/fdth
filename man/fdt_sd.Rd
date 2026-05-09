\name{fdt_sd}
\alias{fdt_sd}
\alias{fdt_sd.default}
\alias{fdt_sd.fdt}
\alias{fdt_sd.fdt.multiple}
\alias{sd.fdt}
\alias{sd.fdt.multiple}

\title{
  Standard deviation of frequency distribution table (numerical variable)
}

\description{
  S3 methods for the standard deviation of a \code{fdt}. \cr
  Useful to estimate the standard deviation (when the real data vector is not known) from a previous \code{fdt}.

  These functions are named \code{fdt_sd} (instead of \code{sd}) so that loading \pkg{fdth} does not mask \code{\link[stats]{sd}} from \pkg{stats}.
}

\usage{
fdt_sd(x, \dots)

\method{fdt_sd}{default}(x, \dots)

\method{fdt_sd}{fdt}(x, \dots)

\method{fdt_sd}{fdt.multiple}(x, \dots)
}

\arguments{
  \item{x}{a \code{fdt} (simple or multiple) object, or a numeric vector (passed to \code{\link[stats]{sd}}).}
  \item{\dots}{further arguments passed to \code{\link[stats]{sd}} for the default method.}
}

\details{\code{fdt_sd.fdt} calculates the value of the standard deviation based on a known formula.
\code{fdt_sd.fdt.multiple} calls \code{fdt_sd.fdt} for each variable, that is, each column of the data.frame.
}

\value{\code{fdt_sd.fdt} returns a numeric vector containing the value of the standard deviation of the \code{fdt}.
 \code{fdt_sd.fdt.multiple} returns a list, where each element is a numeric vector
 containing the value of the standard deviation of the \code{fdt} for each variable.
}

\seealso{\code{\link{fdt_var}}, \code{\link{mean.fdt}}.
}

\author{
  Faria, J. C. \cr
  Allaman, I. B \cr
  Jelihovschi, E. G.
}

\examples{
mdf <- data.frame(x=rnorm(1e3,
                          20,
                          2),
                  y=rnorm(1e3,
                          30,
                          3),
                  z=rnorm(1e3,
                          40,
                          4))

head(mdf)

# From a data.frame
apply(mdf,
      2,
      sd)

# From a fdt object
fdt_sd(fdt(mdf))
}
