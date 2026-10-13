# Optimize energy-based sound event detection

Optimize energy-based sound event detection under different tuning
parameter combinations

## Usage

``` r
optimize_energy_detector(
  reference,
  files = NULL,
  threshold = 5,
  peak.amplitude = 0,
  hop.size = 11.6,
  wl = NULL,
  smooth = 5,
  hold.time = 0,
  min.duration = NULL,
  max.duration = NULL,
  thinning = 1,
  cores = 1,
  pb = TRUE,
  by.sound.file = FALSE,
  bp = NULL,
  path = ".",
  previous.output = NULL,
  envelopes = NULL,
  macro.average = FALSE,
  min.overlap = 0.5
)
```

## Arguments

- reference:

  Selection table (using the warbleR package's format, see
  [`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.html))
  or data frame with columns for sound file name (sound.files),
  selection number (selec), and start and end time of sound event (start
  and end). **It must contain the reference selections that will be used
  for detection optimization**.

- files:

  Character vector indicating the sound files that will be analyzed.
  Optional. If not supplied the function will work on the sound files in
  'reference'. It can be used to include sound files with no target
  sound events. Supported file formats:'.wav', '.mp3', '.flac' and
  '.wac'. If not supplied the function will work on all sound files (in
  the supported format) in 'path'.

- threshold:

  A numeric vector specifying the amplitude threshold for detecting
  sound events (in %). Default is 5. **Several values can be supplied
  for optimization**.

- peak.amplitude:

  Numeric vector of length 1 with the minimum peak amplitude value. A
  detection below that value would be excluded. Peak amplitude is the
  maximum sound pressure level (in decibels) across the sound event (see
  [`sound_pressure_level`](https://marce10.github.io/warbleR/reference/sound_pressure_level.html)).
  This can be useful when expecting higher peak amplitude in the target
  sound events compared to non-target sound events or when keeping only
  the best examples of the target sound events (i.e. high precision and
  low recall). Default is 0. **Several values can be supplied for
  optimization**.

- hop.size:

  A numeric vector of length 1 specifying the time window duration (in
  ms). Default is 11.6 ms, which is equivalent to 512 wl for a 44.1 kHz
  sampling rate. Ignored if 'wl' is supplied.

- wl:

  A numeric vector of length 1 specifying the window length of the
  spectrogram. Default is `NULL`. If supplied, 'hop.size' is ignored.
  Used internally for bandpass filtering (so only applied when 'bp' is
  supplied).

- smooth:

  A numeric vector of length 1 to smooth the amplitude envelope with a
  sum smooth function. It controls the time 'neighborhood' (in ms) in
  which amplitude samples are smoothed (i.e. averaged with neighboring
  samples). Default is 5. 0 means no smoothing is applied. Note that
  smoothing is applied before thinning (see 'thinning' argument). The
  function
  [`envelope`](https://marce10.github.io/warbleR/reference/envelope.html)
  is used internally which is analogous to sum smoothing in
  [`env`](https://rdrr.io/pkg/seewave/man/env.html). This argument is
  used internally by
  [`get_envelopes`](https://docs.ropensci.org/ohun/reference/get_envelopes.md).
  **Several values can be supplied for optimization**.

- hold.time:

  Numeric vector of length 1. Specifies the time range (in ms) at which
  selections will be merged (i.e. if 2 selections are separated by less
  than the specified 'hold.time' they will be merged in to a single
  selection). Default is `0` (no hold time applied). **Several values
  can be supplied for optimization**.

- min.duration:

  Numeric vector giving the shortest duration (in ms) of the sound
  events to be detected. It removes sound events below that threshold.
  **Several values can be supplied for optimization**.

- max.duration:

  Numeric vector giving the longest duration (in ms) of the sound events
  to be detected. It removes sound events above that threshold.
  **Several values can be supplied for optimization**.

- thinning:

  Numeric vector in the range 0~1 indicating the proportional reduction
  of the number of samples used to represent amplitude envelopes (i.e.
  the thinning of the envelopes). Usually amplitude envelopes have many
  more samples than those needed to accurately represent amplitude
  variation in time, which affects the size of the output (usually very
  large R objects / files). Default is `1` (no thinning). Higher
  sampling rates may afford higher size reduction (e.g. lower thinning
  values). Reduction is conducted by interpolation using
  [`approx`](https://rdrr.io/r/stats/approxfun.html). Note that thinning
  may decrease time precision, and the higher the thinning the less
  precise the time detection. **Several values can be supplied for
  optimization**.

- cores:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar and messages. Default is
  `TRUE`.

- by.sound.file:

  Logical argument to control whether performance diagnostics are
  summarized across sound files (when `by.sound.file = FALSE` and more
  than 1 sound file is included in 'reference') or shown separated by
  sound file. Default is `FALSE`.

- bp:

  Numeric vector of length 2 giving the lower and upper limits of a
  frequency bandpass filter (in kHz). Default is `NULL`. This argument
  is used internally by
  [`get_envelopes`](https://docs.ropensci.org/ohun/reference/get_envelopes.md).
  Not used if 'envelopes' are supplied. Bandpass is done using the
  function [`ffilter`](https://rdrr.io/pkg/seewave/man/ffilter.html),
  which applies a short-term Fourier transformation to first create a
  spectrogram in which the target frequencies are filtered and then is
  back transformed into a wave object using a reverse Fourier
  transformation.

- path:

  Character string containing the directory path where the sound files
  are located. The current working directory is used as default.

- previous.output:

  Data frame with the output of a previous run of this function. This
  will be used to include previous results in the new output and avoid
  recalculating detection performance for parameter combinations
  previously evaluated.

- envelopes:

  An object of class 'envelopes' (generated by
  [`get_envelopes`](https://docs.ropensci.org/ohun/reference/get_envelopes.md))
  containing the amplitude envelopes of the sound files to be analyzed.
  If 'files' and 'envelopes' are not supplied then the function will
  work on all supported format sound files in the working directory.

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

- min.overlap:

  Numeric. Controls the minimum amount of overlap required for a
  detection and a reference sound for it to be counted as true positive.
  Default is 0.5. Overlap is measured as intersection over union.

## Value

A data frame in which each row shows the result of a detection job with
a particular combination of tuning parameters (included in the data
frame). It also includes the following diagnostic metrics:

- `true.positives`: number of sound events in 'reference' that
  correspond to any detection. Matching is defined as some degree of
  overlap in time. In a perfect detection routine it should be equal to
  the number of rows in 'reference'.

- `false.positives`: number of detections that don't match any of the
  sound events in 'reference'. In a perfect detection routine it should
  be 0.

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
  were not split or merged. Only included when
  `time.diagnostics = TRUE`.

- `duty.cycle`: proportion of a sound file in which sounds were
  detected. Only included when `time.diagnostics = TRUE` and `path` is
  supplied.

- `recall`: Proportion of sound events in 'reference' that were
  detected. In a perfect detection routine it should be 1.

- `precision`: Proportion of detections that correspond to sound events
  in 'reference'. In a perfect detection routine it should be 1.

## Details

This function takes a selections data frame or 'selection_table'
('reference') and estimates the detection performance of an energy
detector under different detection parameter combinations. This is done
by comparing the position in time of the detection to those of the
reference selections in 'reference'. The function returns several
diagnostic metrics to allow users to determine which parameter values
provide a detection that more closely matches the selections in
'reference'. Those parameters can be later used for performing a more
efficient detection using
[`energy_detector`](https://docs.ropensci.org/ohun/reference/energy_detector.md).

## References

Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C.,
Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. (2023). ohun: An R
package for diagnosing and optimizing automatic sound event detection.
Methods in Ecology and Evolution, 14, 2259–2271.
https://doi.org/10.1111/2041-210X.14170

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>).

## Examples

``` r
# \donttest{
# Save example files into temporary working directory
data("lbh1", "lbh2", "lbh_reference")
tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

# using smoothing and minimum duration
optimize_energy_detector(
  reference = lbh_reference, path = tempdir(),
  threshold = c(6, 10), smooth = 6.8, bp = c(2, 9), hop.size = 6.8,
  min.duration = 90
)
#> 2 combinations will be evaluated:
#>   threshold peak.amplitude smooth hold.time min.duration max.duration thinning
#> 1         6              0    6.8         0           90          Inf        1
#> 2        10              0    6.8         0           90          Inf        1
#>   detections true.positives false.positives false.negatives splits merges
#> 1         19             19               0               0      0      0
#> 2         19             19               0               0      0      0
#>     overlap mean.duration.true.positives mean.duration.false.positives
#> 1 0.8526227                          164                            NA
#> 2 0.9081851                          147                            NA
#>   mean.duration.false.negatives proportional.duration.true.positives duty.cycle
#> 1                            NA                             1.153234  0.3113362
#> 2                            NA                             1.032732  0.2789096
#>   recall precision f.score
#> 1      1         1       1
#> 2      1         1       1

# with thinning and smoothing
optimize_energy_detector(
  reference = lbh_reference, path = tempdir(),
  threshold = c(6, 10, 15), smooth = c(7, 10), thinning = c(0.1, 0.01),
  bp = c(2, 9), hop.size = 6.8, min.duration = 90
)
#> 12 combinations will be evaluated:
#>    threshold peak.amplitude smooth hold.time min.duration max.duration thinning
#> 1          6              0      7         0           90          Inf     0.10
#> 2         10              0      7         0           90          Inf     0.10
#> 3         15              0      7         0           90          Inf     0.10
#> 4          6              0     10         0           90          Inf     0.10
#> 5         10              0     10         0           90          Inf     0.10
#> 6         15              0     10         0           90          Inf     0.10
#> 7          6              0      7         0           90          Inf     0.01
#> 8         10              0      7         0           90          Inf     0.01
#> 9         15              0      7         0           90          Inf     0.01
#> 10         6              0     10         0           90          Inf     0.01
#> 11        10              0     10         0           90          Inf     0.01
#> 12        15              0     10         0           90          Inf     0.01
#>    detections true.positives false.positives false.negatives splits merges
#> 1          19             19               0               0      0      0
#> 2          19             19               0               0      0      0
#> 3          19             19               0               0      0      0
#> 4          19             19               0               0      0      0
#> 5          19             19               0               0      0      0
#> 6          19             19               0               0      0      0
#> 7          19             19               0               0      0      0
#> 8          19             19               0               0      0      0
#> 9          19             19               0               0      0      0
#> 10         19             19               0               0      0      0
#> 11         19             19               0               0      0      0
#> 12         19             19               0               0      0      0
#>      overlap mean.duration.true.positives mean.duration.false.positives
#> 1  0.8494709                          165                            NA
#> 2  0.9065211                          147                            NA
#> 3  0.9227890                          136                            NA
#> 4  0.8300895                          169                            NA
#> 5  0.9018598                          149                            NA
#> 6  0.9192277                          139                            NA
#> 7  0.8265783                          164                            NA
#> 8  0.8872974                          149                            NA
#> 9  0.9075075                          137                            NA
#> 10 0.8103066                          171                            NA
#> 11 0.8863643                          150                            NA
#> 12 0.9044356                          140                            NA
#>    mean.duration.false.negatives proportional.duration.true.positives
#> 1                             NA                            1.1573464
#> 2                             NA                            1.0337601
#> 3                             NA                            0.9542338
#> 4                             NA                            1.1921639
#> 5                             NA                            1.0501397
#> 6                             NA                            0.9780232
#> 7                             NA                            1.1575802
#> 8                             NA                            1.0474697
#> 9                             NA                            0.9631188
#> 10                            NA                            1.2023828
#> 11                            NA                            1.0558039
#> 12                            NA                            0.9802918
#>    duty.cycle recall precision f.score
#> 1   0.3125000      1         1       1
#> 2   0.2792090      1         1       1
#> 3   0.2581187      1         1       1
#> 4   0.3218886      1         1       1
#> 5   0.2836085      1         1       1
#> 6   0.2642870      1         1       1
#> 7   0.3124432      1         1       1
#> 8   0.2829246      1         1       1
#> 9   0.2602180      1         1       1
#> 10  0.3247048      1         1       1
#> 11  0.2851953      1         1       1
#> 12  0.2647593      1         1       1

# by sound file
(opt_ed <- optimize_energy_detector(
  reference = lbh_reference,
  path = tempdir(), threshold = c(6, 10, 15), smooth = 6.8, bp = c(2, 9),
  hop.size = 6.8, min.duration = 90, by.sound.file = TRUE
))
#> 3 combinations will be evaluated:
#>   sound.files threshold peak.amplitude smooth hold.time min.duration
#> 1    lbh2.wav         6              0    6.8         0           90
#> 2    lbh1.wav         6              0    6.8         0           90
#> 3    lbh2.wav        10              0    6.8         0           90
#> 4    lbh1.wav        10              0    6.8         0           90
#> 5    lbh2.wav        15              0    6.8         0           90
#> 6    lbh1.wav        15              0    6.8         0           90
#>   max.duration thinning detections true.positives false.positives
#> 1          Inf        1          9              9               0
#> 2          Inf        1         10             10               0
#> 3          Inf        1          9              9               0
#> 4          Inf        1         10             10               0
#> 5          Inf        1          9              9               0
#> 6          Inf        1         10             10               0
#>   false.negatives splits merges mean.duration.true.positives
#> 1               0      0      0                          159
#> 2               0      0      0                          168
#> 3               0      0      0                          142
#> 4               0      0      0                          151
#> 5               0      0      0                          128
#> 6               0      0      0                          142
#>   mean.duration.false.positives mean.duration.false.negatives   overlap
#> 1                            NA                            NA 0.8044589
#> 2                            NA                            NA 0.8959702
#> 3                            NA                            NA 0.8747964
#> 4                            NA                            NA 0.9382350
#> 5                            NA                            NA 0.9221066
#> 6                            NA                            NA 0.9235634
#>   proportional.duration.true.positives duty.cycle recall precision f.score
#> 1                            1.2125978  0.2869232      1         1       1
#> 2                            1.0998059  0.3357491      1         1       1
#> 3                            1.0803862  0.2556395      1         1       1
#> 4                            0.9898431  0.3021796      1         1       1
#> 5                            0.9759279  0.2309227      1         1       1
#> 6                            0.9322323  0.2845922      1         1       1

# summarize
summarize_diagnostic(opt_ed)
#>   threshold peak.amplitude smooth hold.time min.duration max.duration thinning
#> 1         6              0    6.8         0           90          Inf        1
#> 2        10              0    6.8         0           90          Inf        1
#> 3        15              0    6.8         0           90          Inf        1
#>   detections true.positives false.positives false.negatives splits merges
#> 1         19             19               0               0      0      0
#> 2         19             19               0               0      0      0
#> 3         19             19               0               0      0      0
#>     overlap recall precision f.score
#> 1 0.8526227      1         1       1
#> 2 0.9081851      1         1       1
#> 3 0.9228733      1         1       1

# using hold time
(op_ed <- optimize_energy_detector(
  reference = lbh_reference,
  threshold = 10, hold.time = c(100, 150), bp = c(2, 9), hop.size = 6.8,
  path = tempdir()
))
#> 2 combinations will be evaluated:
#>   threshold peak.amplitude smooth hold.time min.duration max.duration thinning
#> 1        10              0      5       100         -Inf          Inf        1
#> 2        10              0      5       150         -Inf          Inf        1
#>   detections true.positives false.positives false.negatives splits merges
#> 1         19             19               0               0      0      0
#> 2         19             19               0               0      0      0
#>     overlap mean.duration.true.positives mean.duration.false.positives
#> 1 0.8774537                          152                            NA
#> 2 0.8774537                          152                            NA
#>   mean.duration.false.negatives proportional.duration.true.positives duty.cycle
#> 1                            NA                              1.06869  0.2890185
#> 2                            NA                              1.06869  0.2890185
#>   recall precision f.score
#> 1      1         1       1
#> 2      1         1       1

# including previous output in new call
optimize_energy_detector(
  reference = lbh_reference, threshold = 10,
  hold.time = c(50, 200), previous.output = op_ed, smooth = 6.8,
  bp = c(2, 9), hop.size = 7, path = tempdir()
)
#> 2 combinations will be evaluated:
#>   threshold peak.amplitude smooth hold.time min.duration max.duration thinning
#> 1        10              0    5.0       100         -Inf          Inf        1
#> 2        10              0    5.0       150         -Inf          Inf        1
#> 3        10              0    6.8        50         -Inf          Inf        1
#> 4        10              0    6.8       200         -Inf          Inf        1
#>   detections true.positives false.positives false.negatives splits merges
#> 1         19             19               0               0      0      0
#> 2         19             19               0               0      0      0
#> 3         20             19               1               0      0      0
#> 4         19             19               0               0      0      0
#>     overlap mean.duration.true.positives mean.duration.false.positives
#> 1 0.8774537                          152                            NA
#> 2 0.8774537                          152                            NA
#> 3 0.8897689                          150                             2
#> 4 0.8763392                          153                            NA
#>   mean.duration.false.negatives proportional.duration.true.positives duty.cycle
#> 1                            NA                             1.068690  0.2890185
#> 2                            NA                             1.068690  0.2890185
#> 3                            NA                             1.056741  0.2855536
#> 4                            NA                             1.074756  0.2905922
#>   recall precision  f.score
#> 1      1      1.00 1.000000
#> 2      1      1.00 1.000000
#> 3      1      0.95 0.974359
#> 4      1      1.00 1.000000

# having an extra file in files (simulating a file that should have no detections)
sub_reference <- lbh_reference[lbh_reference$sound.files != "lbh1.wav", ]

optimize_energy_detector(
  reference = sub_reference, files = unique(lbh_reference$sound.files),
  threshold = 10, hold.time = c(1, 150), bp = c(2, 9), smooth = 6.8,
  hop.size = 7, path = tempdir()
)
#> 2 combinations will be evaluated:
#>   threshold peak.amplitude smooth hold.time min.duration max.duration thinning
#> 1        10              0    6.8         1         -Inf          Inf        1
#> 2        10              0    6.8       150         -Inf          Inf        1
#>   detections true.positives false.positives false.negatives splits merges
#> 1         28              9              19               0      0      0
#> 2         19              9              10               0      0      0
#>     overlap mean.duration.true.positives mean.duration.false.positives
#> 1 0.8677330                          143                            81
#> 2 0.8543355                          146                           160
#>   mean.duration.false.negatives proportional.duration.true.positives duty.cycle
#> 1                            NA                             1.089011  0.2829051
#> 2                            NA                             1.106874  0.2905922
#>   recall precision   f.score
#> 1      1 0.3214286 0.4864865
#> 2      1 0.4736842 0.6428571
# }
```
