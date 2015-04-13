### fdth

This is the development code of the R package **fdth**.
You should use it if you want to contribute to its development:
testing unreleased versions, fixing bugs, writing code, etc.

To download, check and build it do the following in a terminal emulator:

> git clone  git://github.com/jcfaria/fdth.git

> or

> git clone https://jcfaria@github.com/jcfaria/fdth.git

After to clone it, to check, build and install do the following:
> R CMD check fdth

> R CMD build fdth

> R CMD INSTALL fdth_X.X-X.tar.gz

Or, if you could install directly as:

> require(devtools)

> install_github("fdth", "jcfaria")

The stable version of this package is available at:
http://cran.r-project.org/web/packages/fdth/index.html
