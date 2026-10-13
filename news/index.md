# Changelog

## ohun 1.0.5

CRAN release: 2026-10-08

#### NEW FEATURES

- [`split_acoustic_data()`](https://docs.ropensci.org/ohun/reference/split_acoustic_data.md)
  returns the name and duration of files with a duration less than the
  specified clip duration
- new argument `save.txt` in
  [`energy_detector()`](https://docs.ropensci.org/ohun/reference/energy_detector.md)
  and
  [`template_detector()`](https://docs.ropensci.org/ohun/reference/template_detector.md)
  to save selection table (sensu Raven Pro) txt files for each sound
  file to facilitate resuming process
- new argument `resume` in
  [`energy_detector()`](https://docs.ropensci.org/ohun/reference/energy_detector.md)
  and
  [`template_detector()`](https://docs.ropensci.org/ohun/reference/template_detector.md)
  to control whether previously saved ‘.txt’ files are read back in
  instead of recomputing detection; `save.txt` alone no longer causes
  previous results to be silently reused when parameters change

#### BUG FIXES

- [`reassemble_detection()`](https://docs.ropensci.org/ohun/reference/reassemble_detection.md)
  no longer produces duplicated ‘selec’ labels when an original sound
  file was split into more than one clip
- [`split_acoustic_data()`](https://docs.ropensci.org/ohun/reference/split_acoustic_data.md)
  no longer leaves a stale value in `options('unsplit_sound_files')`
  from a previous call
- parallel clusters created internally on Windows (when `cores > 1`) are
  now properly stopped after use

## ohun 1.0.4

CRAN release: 2025-10-22

#### MINOR IMPROVEMENTS

- ‘overwrite’ argument added to
  [`split_acoustic_data()`](https://docs.ropensci.org/ohun/reference/split_acoustic_data.md)
  to allow overwriting existing files

## ohun 1.0.3

CRAN release: 2025-07-22

#### NEW FEATURES

- New function
  [`reassemble_detection()`](https://docs.ropensci.org/ohun/reference/reassemble_detection.md)
  to reassembles detections made on clips so they refer to the original
  sound files

#### MINOR IMPROVEMENTS

- Update names of functions from package warbleR those in latest
  versions
- Replace [`sapply()`](https://rdrr.io/r/base/lapply.html) with
  [`vapply()`](https://rdrr.io/r/base/lapply.html)
- Fix bug when true positive are set to false positives based on solving
  ambiguous detection using maximum bipartite graph matching

## ohun 1.0.2

CRAN release: 2024-08-19

Update requested by CRAN.

## ohun 1.0.1

CRAN release: 2023-11-17

Update requested by CRAN.

## ohun 1.0.0

CRAN release: 2023-09-23

#### NEW FEATURES

- New function
  [`plot_detection()`](https://docs.ropensci.org/ohun/reference/plot_detection.md)
  to visually inspect detections

#### MINOR IMPROVEMENTS

- `sp` package replaced by `sf`
- Replace [`sapply()`](https://rdrr.io/r/base/lapply.html) with
  [`vapply()`](https://rdrr.io/r/base/lapply.html)
- performance indices name changes: split.positives to splits,
  merged.positives to merges, proportional.overlap.to.true.positives to
  overlap and f1.score to f.score
- [`label_detection()`](https://docs.ropensci.org/ohun/reference/label_detection.md)
  renamed
  [`consensus_detection()`](https://docs.ropensci.org/ohun/reference/consensus_detection.md)

## ohun 0.1.0 (2022-12-19)

CRAN release: 2022-12-19

#### NEW FEATURES

- released to CRAN
