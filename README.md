ohun: optimizing sound event detection
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Dependencies](https://tinyverse.netlify.com/badge/ohun)](https://cran.r-project.org/package=ohun)
[![Project Status: Active The project has reached a stable, usable state
and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-%3E=%203.2.1-6666ff.svg)](https://cran.r-project.org/)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.1-orange.svg?style=flat-square)](commits/develop)
[![Last-changedate](https://img.shields.io/badge/last%20change-2023--07--03-yellowgreen.svg)](/commits/master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ohun)](https://cran.r-project.org/package=ohun)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/ohun)](https://cranlogs.r-pkg.org/badges/grand-total/ohun)
[![Codecov test
coverage](https://codecov.io/gh/maRce10/ohun/branch/master/graph/badge.svg)](https://app.codecov.io/gh/maRce10/ohun?branch=master)
[![R-CMD-check](https://github.com/maRce10/ohun/workflows/R-CMD-check/badge.svg)](https://github.com/maRce10/ohun/actions)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/568_status.svg)](https://github.com/ropensci/software-review/issues/568)
<!-- badges: end -->

<img src="vignettes/ohun_sticker.png" alt="ohun logo" align="right" width = "25%" height="25%"/>

[ohun](https://github.com/maRce10/ohun) is intended to facilitate the
automated detection of sound events, providing functions to diagnose and
optimize detection routines.

The main features of the package are:

- The use of reference annotations for detection diagnostic and
  optimization
- The use of signal detection theory indices to evaluate detection
  performance

The package offers functions for:

- Curate references and acoustic data sets
- Diagnose detection performance
- Optimize detection routines based on reference annotations
- Energy-based detection
- Template-based detection

All functions allow the parallelization of tasks, which distributes the
tasks among several processors to improve computational efficiency. The
package works on sound files in ‘.wav’, ‘.mp3’, ‘.flac’ and ‘.wac’
format.

Install/load the package from CRAN as follows:

``` r
# From CRAN would be
install.packages("ohun")

#load package
library(ohun)
```

To install the latest developmental version from
[github](https://github.com/) you will need the R package
[remotes](https://cran.r-project.org/package=remotes):

``` r
remotes::install_github("maRce10/ohun")

#load package
library(ohun)
```

Take a look at the vignettes for an overview of the main features of the
packages:

- [Optimizing sound event
  detection](https://marce10.github.io/ohun/articles/intro_to_ohun.html)
- [Energy-based
  detection](https://marce10.github.io/ohun/articles/energy_based_detection.html)
- [Template-based
  detection](https://marce10.github.io/ohun/articles/template_based_detection.html)

------------------------------------------------------------------------

Please cite [ohun](https://github.com/maRce10/ohun) as follows:

Araya-Salas, M. (2022), ohun: diagnosing and optimizing automated sound
event detection. R package version 0.1.1.
