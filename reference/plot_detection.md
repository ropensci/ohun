# Plot detection and reference annotations

`plot_detection` plots detection and reference annotations as rectangles
along the time axis

## Usage

``` r
plot_detection(
  reference,
  detection,
  mid.point = FALSE,
  size = 20,
  positions = c(1, 2)
)
```

## Arguments

- reference:

  Data frame or 'selection.table' (following the warbleR package format)
  with the reference selections (start and end of the sound events) to
  be plotted. Must contain at least the following columns:
  "sound.files", "selec", "start" and "end".

- detection:

  Data frame or 'selection.table' with the detections (start and end of
  the sound events) to be plotted along with the 'reference' selections.
  Must contain at least the following columns: "sound.files", "selec",
  "start" and "end".

- mid.point:

  Logical argument to control if each annotation is shown as a rectangle
  with fixed width centered at the mid point of the time position (if
  `TRUE`) or the true time range of the annotations is used (if `FALSE`,
  default). 'mid.point' can be useful to make visible annotations in
  very long sound files that would otherwise look too thin.

- size:

  Numeric. Controls the size of the rectangles if `mid.point = TRUE`.
  Default is 20.

- positions:

  Numeric. Controls the vertical position of the rectangles representing
  annotations. Default is c(1, 2). This can be used to get reference and
  detection annotations closer in the vertical axis. Note that the
  height of rectangles is 0.5.

## Value

A ggplot graph (i.e. an object of class "ggplot").

## Details

The function helps to visualize the match between reference and
detection annotations by plotting them next to each other as rectangles
along the time axis. If the annotations contain data for several sound
files each sound file will be plotted in its own panel. The plot can be
further modified by users using regular ggplot syntax.

## References

Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C.,
Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. (2023). ohun: An R
package for diagnosing and optimizing automatic sound event detection.
Methods in Ecology and Evolution, 14, 2259–2271.
https://doi.org/10.1111/2041-210X.14170

## See also

[`label_spectro`](https://docs.ropensci.org/ohun/reference/label_spectro.md),
[`diagnose_detection`](https://docs.ropensci.org/ohun/reference/diagnose_detection.md)

## Author

Marcelo Araya-Salas <marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  # load data
  data("lbh_reference")

  # mid point and regular size
  plot_detection(
    reference = lbh_reference[-14, ],
    detection = lbh_reference[-1, ], mid.point = TRUE
  )

  # mid point and larger size
  plot_detection(
    reference = lbh_reference[-14, ],
    detection = lbh_reference[-1, ], mid.point = TRUE, size = 25
  )

  # true time rectangles
  plot_detection(
    reference = lbh_reference[-14, ],
    detection = lbh_reference[-1, ]
  )

  # use position to make reference and annotations overlap vertically
  plot_detection(
    reference = lbh_reference[-14, ],
    detection = lbh_reference[-1, ], positions = c(1, 1.4)
  )

  # modified using ggplot
  gg_pd <- plot_detection(
    reference = lbh_reference[-14, ],
    detection = lbh_reference[-1, ], positions = c(1, 1.4)
  )

  gg_pd + ggplot2::theme_classic(base_size = 25)
}
```
