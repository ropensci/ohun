# Summarize temporal and frequency dimensions of annotations and gaps

`summarize_reference` summarizes temporal and frequency dimensions of
annotations and gaps

## Usage

``` r
summarize_reference(
  reference,
  path = NULL,
  by.sound.file = FALSE,
  units = c("ms", "kHz"),
  digits = 2
)
```

## Arguments

- reference:

  Data frame or 'selection.table' (following the warbleR package format)
  with the reference selections (start and end of the sound events) that
  will be used to evaluate the performance of the detection, represented
  by those selections in 'detection'. Must contained at least the
  following columns: "sound.files", "selec", "start" and "end". If
  frequency range columns are included ("bottom.freq" and "top.freq")
  these are also used to characterize reference selections.

- path:

  Character string containing the directory path where the sound files
  are located. If supplied then duty cycle and peak amplitude features
  are returned. These features are more helpful for tuning an
  energy-based detection. Default is `NULL`.

- by.sound.file:

  Logical argument to control whether features are summarized across
  sound files (when `by.sound.file = FALSE`, and more than 1 sound file
  is included in 'reference') or shown separated by sound file. Default
  is `FALSE`.

- units:

  A character vector of length 2 with the units to be used for time and
  frequency parameters, in that order. Default is `c("ms", "kHz")`. It
  can also take 's' and 'Hz'.

- digits:

  Numeric vector of length 1 with the number of decimals to include.
  Default is 2.

## Value

The function returns the mean, minimum and maximum duration of
selections and gaps (time intervals between selections) and of the
number of annotations by sound file. If frequency range columns are
included in the reference table (i.e. "bottom.freq" and "top.freq") the
minimum bottom frequency ('min.bottom.freq') and the maximum top
frequency ('max.top.freq') are also estimated. Finally, if the path to
the sound files in 'reference' is supplied the duty cycle (fraction of a
sound file corresponding to target sound events) and peak amplitude
(highest amplitude in a detection) are also returned. If \`by.sound.file
= FALSE\` a matrix with features in rows is returned. Otherwise a data
frame is returned in which each row corresponds to a sound file. By
default, time features are returned in 'ms' while frequency features in
'kHz' (but see 'units' argument).

## Details

The function extracts quantitative features from reference tables that
can inform the range of values to be used in an energy-based detection
optimization routine. Features related to selection duration can be used
to set the 'max.duration' and 'min.duration' values, frequency related
features can inform bandpass values, gap related features inform hold
time values and duty cycle can be used to evaluate performance.

## References

Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C.,
Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. (2023). ohun: An R
package for diagnosing and optimizing automatic sound event detection.
Methods in Ecology and Evolution, 14, 2259–2271.
https://doi.org/10.1111/2041-210X.14170

## See also

[`optimize_energy_detector`](https://docs.ropensci.org/ohun/reference/optimize_energy_detector.md),
[`optimize_template_detector`](https://docs.ropensci.org/ohun/reference/optimize_template_detector.md)

## Author

Marcelo Araya-Salas <marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  # load data and save example files into temporary working directory
  data("lbh1", "lbh2", "lbh_reference")
  tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
  tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

  # summary across sound files
  summarize_reference(reference = lbh_reference, path = tempdir())

  # summary by sound file
  summarize_reference(reference = lbh_reference, by.sound.file = TRUE, path = tempdir())
}
#>   sound.files min.sel.duration mean.sel.duration max.sel.duration
#> 1    lbh2.wav           117.96            131.45           139.08
#> 2    lbh1.wav           140.84            152.64           163.73
#>   min.gap.duration mean.gap.duration max.gap.duration annotations
#> 1           406.68            446.09           484.14           9
#> 2           322.16            352.29           514.08          10
#>   min.bottom.freq mean.bottom.freq max.bottom.freq min.top.freq mean.top.freq
#> 1            2.16             2.27            2.37         8.49          8.82
#> 2            1.81             1.96            2.09         8.49          8.82
#>   max.top.freq duty.cycle min.peak.amplitude mean.peak.amplitude
#> 1         9.08       0.24              73.76               76.01
#> 2         9.53       0.31              85.21               86.60
#>   max.peak.amplitude
#> 1              77.65
#> 2              88.03
```
