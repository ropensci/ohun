# Acoustic templates correlator using time-frequency cross-correlation

`template_correlator` estimates templates cross-correlation across
multiple sound files.

## Usage

``` r
template_correlator(
  templates,
  files = NULL,
  hop.size = 11.6,
  wl = NULL,
  ovlp = 0,
  wn = "hanning",
  cor.method = "pearson",
  cores = 1,
  path = ".",
  pb = TRUE,
  type = "fourier",
  fbtype = "mel",
  ...
)
```

## Arguments

- templates:

  'selection_table', 'extended_selection_table' (warbleR package's
  formats, see
  [`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.html))
  or data frame with time and frequency information of the sound
  event(s) to be used as templates (1 template per row). The object must
  contain columns for sound files (sound.files), selection number
  (selec), and start and end time of sound event (start and end). If
  frequency range columns are included ('bottom.freq' and 'top.freq', in
  kHz) the correlation will be run on those frequency ranges. All
  templates must have the same sampling rate and both templates and
  'files' (in which to find templates) must also have the same sampling
  rate.

- files:

  Character vector with name of the files in which to run the
  cross-correlation with the supplied template(s). Supported file
  formats:'.wav', '.mp3', '.flac' and '.wac'. If not supplied the
  function will work on all sound files (in the supported formats) in
  'path'.

- hop.size:

  A numeric vector of length 1 specifying the time window duration (in
  ms). Default is 11.6 ms, which is equivalent to 512 wl for a 44.1 kHz
  sampling rate. Ignored if 'wl' is supplied.

- wl:

  A numeric vector of length 1 specifying the window length of the
  spectrogram. Default is `NULL`. If supplied, 'hop.size' is ignored.

- ovlp:

  Numeric vector of length 1 specifying % of overlap between two
  consecutive windows, as in
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html). Default
  is 0. High values of ovlp slow down the function but may produce more
  accurate results.

- wn:

  A character vector of length 1 specifying the window name as in
  [`ftwindow`](https://rdrr.io/pkg/seewave/man/ftwindow.html).

- cor.method:

  A character vector of length 1 specifying the correlation method as in
  [`cor`](https://rdrr.io/r/stats/cor.html).

- cores:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- path:

  Character string containing the directory path where the sound files
  are located. The current working directory is used as default.

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- type:

  A character vector of length 1 specifying the type of
  cross-correlation: "fourier" (i.e. spectrographic cross-correlation
  using Fourier transform; internally using
  [`spectro`](https://rdrr.io/pkg/seewave/man/spectro.html); default),
  "mfcc" (auditory scale coefficient matrix cross-correlation;
  internally using
  [`melfcc`](https://rdrr.io/pkg/tuneR/man/melfcc.html)) or
  "mel-auditory" (cross-correlation of auditory spectrum, i.e. spectrum
  after transformation to an auditory scale; internally using
  [`melfcc`](https://rdrr.io/pkg/tuneR/man/melfcc.html)). The argument
  'fbtype' controls the auditory scale to be used. Note that the last 2
  methods have not been widely used in this context so can be regarded
  as experimental.

- fbtype:

  Character vector indicating the auditory frequency scale to use:
  "mel", "bark", "htkmel", "fcmel".

- ...:

  Additional arguments to be passed to
  [`melfcc`](https://rdrr.io/pkg/tuneR/man/melfcc.html) for further
  customization when using auditory scales.

## Value

The function returns an object of class 'template_correlations' which is
a list with the correlation scores for each combination of templates and
files. 'template_correlations' objects must be used to infer sound event
occurrences using
[`template_detector`](https://docs.ropensci.org/ohun/reference/template_detector.md).
Correlation scores for a single template/sound file combination can be
plotted with
[`label_spectro`](https://docs.ropensci.org/ohun/reference/label_spectro.md).

## Details

This function calculates the similarity of acoustic templates across
sound files by means of time-frequency cross-correlation. Fourier
spectrograms or time-frequency representations from auditory scales
(including cepstral coefficients) can be used. Several templates can be
run over several sound files. Note that template-based detection is
divided into two steps: template correlation (using this function) and
template detection (or peak detection as it infers detection based on
peak correlation scores, using the function
[`template_detector`](https://docs.ropensci.org/ohun/reference/template_detector.md)).
So the output of this function (an object of class
'template_correlations') must be input into
[`template_detector`](https://docs.ropensci.org/ohun/reference/template_detector.md)
for inferring sound event occurrences.
[`optimize_template_detector`](https://docs.ropensci.org/ohun/reference/optimize_template_detector.md)
can be used to optimize template detection.

## References

Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C.,
Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. (2023). ohun: An R
package for diagnosing and optimizing automatic sound event detection.
Methods in Ecology and Evolution, 14, 2259–2271.
https://doi.org/10.1111/2041-210X.14170

Khanna H., Gaunt S.L.L. & McCallum D.A. (1997). Digital spectrographic
cross-correlation: tests of sensitivity. Bioacoustics 7(3): 209-234.

Lyon, R. H., & Ordubadi, A. (1982). Use of cepstra in acoustical signal
analysis. Journal of Mechanical Design, 104(2), 303-306.

## See also

[`energy_detector`](https://docs.ropensci.org/ohun/reference/energy_detector.md),
[`template_detector`](https://docs.ropensci.org/ohun/reference/template_detector.md),
[`optimize_template_detector`](https://docs.ropensci.org/ohun/reference/optimize_template_detector.md)

## Author

Marcelo Araya-Salas <marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  # load example data
  data("lbh1", "lbh2", "lbh_reference")

  # save sound files
  tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
  tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

  # create template
  templ <- lbh_reference[4, ]

  # fourier spectrogram
  (tc_fr <- template_correlator(templates = templ, path = tempdir(), type = "fourier"))

  # mel auditory spectrograms
  (tc_ma <- template_correlator(templates = templ, path = tempdir(), type = "mel-auditory"))

  # mfcc spectrograms
  (tc_mfcc <- template_correlator(templates = templ, path = tempdir(), type = "mfcc"))

  # similar results (but not exactly the same) are found with the 3 methods
  # these are the correlation of the correlation vectors
  # fourier vs mel-auditory
  cor(
    tc_fr$`lbh2.wav-4/lbh2.wav`$correlation.scores,
    tc_ma$`lbh2.wav-4/lbh2.wav`$correlation.scores
  )

  # fourier vs mfcc
  cor(
    tc_fr$`lbh2.wav-4/lbh2.wav`$correlation.scores,
    tc_mfcc$`lbh2.wav-4/lbh2.wav`$correlation.scores
  )

  # mel-auditory vs mfcc
  cor(
    tc_ma$`lbh2.wav-4/lbh2.wav`$correlation.scores,
    tc_mfcc$`lbh2.wav-4/lbh2.wav`$correlation.scores
  )

  # using an extended selection table
  templ_est <- warbleR::selection_table(templ,
    extended = TRUE,
    path = tempdir()
  )

  tc_fr_est <- template_correlator(templates = templ_est, path = tempdir(), type = "fourier")

  # produces the same result as templates in a regular data frame
  cor(
    tc_fr$`lbh2.wav-4/lbh2.wav`$correlation.scores,
    tc_fr_est$`lbh2.wav_4-1/lbh2.wav`$correlation.scores
  )
}
#> all selections are OK 
#> 
#> [1] 1
```
