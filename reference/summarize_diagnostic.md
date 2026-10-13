# Summarize detection diagnostics

`summarize_diagnostic` summarizes detection diagnostics

## Usage

``` r
summarize_diagnostic(
  diagnostic,
  time.diagnostics = FALSE,
  macro.average = FALSE
)
```

## Arguments

- diagnostic:

  A data frame with the output of a detection optimization function
  ([`diagnose_detection`](https://docs.ropensci.org/ohun/reference/diagnose_detection.md),
  [`optimize_energy_detector`](https://docs.ropensci.org/ohun/reference/optimize_energy_detector.md)
  or
  [`optimize_template_detector`](https://docs.ropensci.org/ohun/reference/optimize_template_detector.md))

- time.diagnostics:

  Logical argument to control if diagnostics related to the duration of
  the sound events ("mean.duration.true.positives",
  "mean.duration.false.positives", "mean.duration.false.negatives" and
  "proportional.duration.true.positives") are returned (if `TRUE`).
  Default is `FALSE`.

- macro.average:

  Logical argument to control if diagnostics are first calculated for
  each sound file and then averaged across sound files, which can
  minimize the effect of unbalanced sample sizes between sound files. If
  `FALSE` (default) diagnostics are based on aggregated statistics
  irrespective of sound files. The following indices can be estimated by
  macro-averaging: overlap, mean.duration.true.positives,
  mean.duration.false.positives, mean.duration.false.negatives,
  proportional.duration.true.positives, recall and precision (f.score is
  always derived from recall and precision). Note that when applying
  macro-averaging, recall and precision are not derived from the true
  positive, false positive and false negative values returned by the
  function.

## Value

A data frame, similar to the output of a detection optimization function
([`diagnose_detection`](https://docs.ropensci.org/ohun/reference/diagnose_detection.md),
[`optimize_energy_detector`](https://docs.ropensci.org/ohun/reference/optimize_energy_detector.md),
[`optimize_template_detector`](https://docs.ropensci.org/ohun/reference/optimize_template_detector.md))
including the following detection performance diagnostics:

- `detections`: total number of detections

- `true.positives`: number of sound events in 'reference' that
  correspond to any detection. Matching is defined as some degree of
  overlap in time. In a perfect detection routine it should be equal to
  the number of rows in 'reference'.

- `false.positives`: number of detections that don't match (i.e. don't
  overlap with) any of the sound events in 'reference'. In a perfect
  detection routine it should be 0.

- `false.negatives`: number of sound events in 'reference' that were not
  detected (not found in 'detection'. In a perfect detection routine it
  should be 0.

- `splits`: number of detections overlapping reference sounds that also
  overlap with other detections. In a perfect detection routine it
  should be 0.

- `merges`: number of detections that overlap with two or more reference
  sounds. In a perfect detection routine it should be 0.

- `mean.duration.true.positives`: mean duration of true positives (in
  ms). Only included when `time.diagnostics = TRUE`.

- `mean.duration.false.positives`: mean duration of false positives (in
  ms). Only included when `time.diagnostics = TRUE`.

- `mean.duration.false.negatives`: mean duration of false negatives (in
  ms). Only included when `time.diagnostics = TRUE`.

- `overlap`: mean intersection over union overlap of true positives.

- `proportional.duration.true.positives`: ratio of duration of true
  positives to the duration of sound events in 'reference'. In a perfect
  detection routine it should be 1. Based only on true positives that
  were not split or merged.

- `duty.cycle`: proportion of a sound file in which sounds were
  detected. Only included when `time.diagnostics = TRUE` and `path` is
  supplied. Useful when conducting energy-based detection as a perfect
  detection can be obtained with a very low amplitude threshold, which
  will detect everything, but will produce a duty cycle close to 1.

- `recall`: Proportion of sound events in 'reference' that were
  detected. In a perfect detection routine it should be 1.

- `precision`: Proportion of detections that correspond to sound events
  in 'reference'. In a perfect detection routine it should be 1.

- `f.score`: Combines recall and precision as the harmonic mean of these
  two. Provides a single value for evaluating performance. In a perfect
  detection routine it should be 1.

## Details

The function summarizes a detection diagnostic data frame in which
diagnostic parameters are shown split by (typically) a categorical
column, usually sound files. This function is used internally by
[`diagnose_detection`](https://docs.ropensci.org/ohun/reference/diagnose_detection.md).
'splits' and 'merges' are also counted (i.e. counted twice) as
'true.positives'. Therefore "true.positives + false.positives =
detections".

## References

Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C.,
Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. (2023). ohun: An R
package for diagnosing and optimizing automatic sound event detection.
Methods in Ecology and Evolution, 14, 2259–2271.
https://doi.org/10.1111/2041-210X.14170

Mesaros, A., Heittola, T., & Virtanen, T. (2016). Metrics for polyphonic
sound event detection. Applied Sciences, 6(6), 162.

## See also

[`diagnose_detection`](https://docs.ropensci.org/ohun/reference/diagnose_detection.md)

## Author

Marcelo Araya-Salas <marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  # load example selection tables

  data("lbh_reference")

  # run diagnose_detection() by sound file
  diag <- diagnose_detection(
    reference = lbh_reference,
    detection = lbh_reference[-1, ], by.sound.file = TRUE
  )

  # summarize
  summarize_diagnostic(diagnostic = diag)

  # should be the same as this:
  diagnose_detection(
    reference = lbh_reference,
    detection = lbh_reference[-1, ], by.sound.file = FALSE
  )
}
#>   detections true.positives false.positives false.negatives splits merges
#> 1         18             18               0               1      0      0
#>   overlap    recall precision  f.score
#> 1       1 0.9473684         1 0.972973
```
