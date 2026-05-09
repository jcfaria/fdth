\name{fdt_var}
\alias{fdt_var}
\alias{fdt_var.default}
\alias{fdt_var.fdt}
\alias{fdt_var.fdt.multiple}
\alias{var.fdt}
\alias{var.fdt.multiple}

\title{
  Variance of frequency distribution table (numerical variable)
}

\description{
  S3 methods for the variance of a \code{fdt}. \cr
  Useful to estimate the variance (when the real data vector is not known) from a previous \code{fdt}.

  These functions are named \code{fdt_var} (instead of \code{var}) so that loading \pkg{fdth} does not mask \code{\link[stats]{var}} from \pkg{stats}.
}

\usage{
fdt_var(x, \dots)

\method{fdt_var}{default}(x, \dots)

\method{fdt_var}{fdt}(x, \dots)

\method{fdt_var}{fdt.multiple}(x, \dots)
}

\arguments{
  \item{x}{a \code{fdt} (simple or multiple) object, or a numeric vector (passed to \code{\link[stats]{var}}).}
  \item{\dots}{further arguments passed to \code{\link[stats]{var}} for the default method.}
}

\details{\code{fdt_var.fdt} calculates the value of the variance based on a known formula.
\code{fdt_var.fdt.multiple} calls \code{fdt_var.fdt} for each variable, that is, each column of the data.frame.
}

\value{\code{fdt_var.fdt} returns a numeric vector containing the value of the variance of the \code{fdt}.
 \code{fdt_var.fdt.multiple} returns a list, where each element is a numeric vector
 containing the value of the variance of the \code{fdt} for each variable.
}

\seealso{\code{\link{fdt_sd}}, \code{\link{mean.fdt}}.
}

\author{
  Faria, J. C. \cr
  Allaman, I. B \cr
  Jelihovschi, E. G.
}

\examples{
mdf <- data.frame(x=rnorm(1e2,
                          20,
                          2),
                  y=rnorm(1e2,
                          30,
                          3),
                  z=rnorm(1e2,
                          40,
                          4))

head(mdf)

# From a data.frame
apply(mdf,
      2,
      var)

# From a fdt object
fdt_var(fdt(mdf))
}
