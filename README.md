# fdth

`fdth` is an R package for frequency distribution tables, associated histograms, and polygons for numerical and categorical variables.

<!-- Badges -->
[![CRAN status](https://www.r-pkg.org/badges/version/fdth)](https://cran.r-project.org/package=fdth)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/fdth)](https://cran.r-project.org/package=fdth)
[![CRAN checks](https://badges.cranchecks.info/worst/fdth.svg)](https://cran.r-project.org/web/checks/check_results_fdth.html)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License: GPL-2](https://img.shields.io/badge/License-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)

## Key Features

- Frequency distribution tables from vectors, matrices, and data frames.
- Support for numerical (`fdt`) and categorical (`fdt_cat`) variables.
- Associated graphics, including histograms and frequency polygons.
- Summary measures from grouped data (for example, mean, median, and mode).
- Reporting support with `xtable`.

## Installation

Install from CRAN:

```r
install.packages("fdth")
```

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("jcfaria/fdth")
```

## Quick Start

```r
library(fdth)

set.seed(123)
x <- rnorm(100, mean = 20, sd = 3)

ft <- fdt(x)
ft

plot(ft)
summary(ft)

# Most frequent value (mode) from grouped data
mfv(ft)
```

## Project Layout

- `/R`: Core functions and S3 methods.
- `/man`: Reference documentation (`.Rd` files).
- `/tests`: Automated tests (`testthat`).
- `/vignettes`: Tutorials and longer-form documentation.

## Contributing

Contributions are welcome. Open an issue or submit a pull request with:

- Bug fixes and performance improvements.
- Documentation and usability updates.
- New ideas for tables, summaries, and visual workflows.

To check and build locally:

```bash
R CMD check fdth
R CMD build fdth
R CMD INSTALL fdth_X.X-X.tar.gz
```

## Roadmap

- Expand automated tests for edge cases.
- Improve examples for multimodal and grouped-data scenarios.
- Keep documentation aligned with current S3 behavior.

---

Developed by:  
Faria, J. C.; Allaman, I. B.; Jelihovschi, E. G.  
Universidade Estadual de Santa Cruz - UESC  
Departamento de Ciencias Exatas - DCEX  
Ilheus - Bahia - Brasil
