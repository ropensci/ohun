ohun: optimizing sound event detection
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Project Status: Active The project has reached a stable, usable state
and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![R-CMD-check](https://github.com/ropensci/ohun/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/ohun/actions/workflows/R-CMD-check.yaml)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/ohun)](https://cran.r-project.org/package=ohun)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/ohun)](https://cran.r-project.org/package=ohun)
[![Monthly
Downloads](https://cranlogs.r-pkg.org/badges/ohun)](https://cran.r-project.org/package=ohun)
[![Codecov test
coverage](https://codecov.io/gh/maRce10/ohun/branch/master/graph/badge.svg)](https://app.codecov.io/gh/maRce10/ohun?branch=master)
[![Last
Commit](https://img.shields.io/github/last-commit/ropensci/ohun.svg)](https://github.com/ropensci/ohun/commits/master)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/568_status.svg)](https://github.com/ropensci/software-review/issues/568)
<!-- badges: end -->

<img src="vignettes/ohun_sticker.png" alt="ohun logo" align="right" width = "25%" height="25%"/>

[ohun](https://github.com/ropensci/ohun) is intended to facilitate the
automated detection of sound events, providing functions to diagnose and
optimize detection routines. It provides utilities for comparing
detection and annotations of audio events described by frequency and
time boxes.

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

The implementation of detection diagnostics that can be applied to both
built-in detection methods and to those obtained from other software
packages makes the package [ohun](https://github.com/ropensci/ohun) a
useful tool for conducting direct comparisons of the performance of
different routines. In addition, the compatibility of
[ohun](https://github.com/ropensci/ohun) with data formats already used
by other sound analysis R packages (e.g. seewave, warbleR) enables the
integration of [ohun](https://github.com/ropensci/ohun) into more
complex acoustic analysis workflows in a popular programming environment
within the research community.

All functions allow the parallelization of tasks (using the packages
parallel and [pbapply](https://CRAN.R-project.org/package=pbapply)),
which distributes the tasks among several processors to improve
computational efficiency. The package works on sound files in ‘.wav’,
‘.mp3’, ‘.flac’ and ‘.wac’ format.

## Installation

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
remotes::install_github("ropensci/ohun")

#load package
library(ohun)
```

Further system requirements due to the dependency
[seewave](https://cran.r-project.org/package=seewave) may be needed
(e.g. ‘libsndfile’ and ‘fftw3’ on Linux). Take a look at [this archived
page](https://web.archive.org/web/20240521143417/https://rug.mnhn.fr/seewave/inst.html)
for instructions on how to install/troubleshoot these external
dependencies.

## Quick example

The package comes with example data so you can try out a detection
routine right away. The code below runs an energy-based detection on two
sound files and compares it against the reference annotations using
[`diagnose_detection()`](https://docs.ropensci.org/ohun/reference/diagnose_detection.html):

``` r
library(ohun)

# load example data
data("lbh1", "lbh2", "lbh_reference")

# save sound files into a temporary working directory
tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

# detect sound events based on amplitude envelopes
detection <- energy_detector(
  files = c("lbh1.wav", "lbh2.wav"),
  path = tempdir(),
  threshold = 6,
  smooth = 6.8,
  bp = c(2, 9),
  hop.size = 3,
  min.duration = 50
)

# compare the detection against the reference annotations
diagnose_detection(reference = lbh_reference, detection = detection)
```

    ##   detections true.positives false.positives false.negatives splits merges
    ## 1         19             19               0               0      0      0
    ##     overlap recall precision f.score
    ## 1 0.8511668      1         1       1

This returns a set of [signal detection
theory](https://en.wikipedia.org/wiki/Detection_theory) indices
(e.g. recall, precision, F score) that can be used to evaluate and
fine-tune the detection parameters.
[`optimize_energy_detector()`](https://docs.ropensci.org/ohun/reference/optimize_energy_detector.html)
automates this process by testing several parameter combinations at
once.

[`template_detector()`](https://docs.ropensci.org/ohun/reference/template_detector.html)
works in a similar way to `energy_detector()` (same ‘files’/‘path’
arguments and selection table output), but detects sound events by
cross-correlation with a template sound event instead of amplitude
thresholds, so its output can be evaluated with `diagnose_detection()`
and optimized with `optimize_template_detector()` just like above.

## Vignettes

Take a look at the vignettes for a more detailed overview of the main
features of the package:

  - [Optimizing sound event
    detection](https://docs.ropensci.org/ohun/articles/intro_to_ohun.html)
  - [Energy-based
    detection](https://docs.ropensci.org/ohun/articles/energy_based_detection.html)
  - [Template-based
    detection](https://docs.ropensci.org/ohun/articles/template_based_detection.html)

This package has been [peer-reviewed by
rOpenSci](https://github.com/ropensci/software-review/issues/568).

-----

## Citation

Please cite [ohun](https://github.com/ropensci/ohun) as follows:

> Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C.,
> Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. (2023). ohun: An R
> package for diagnosing and optimizing automatic sound event detection.
> Methods in Ecology and Evolution, 14, 2259–2271.
> <https://doi.org/10.1111/2041-210X.14170>
