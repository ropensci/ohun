# Label detections from a sound event detection procedure

`label_detection` labels the performance of a sound event detection
procedure comparing the output selection table to a reference selection
table

## Usage

``` r
label_detection(
  reference,
  detection,
  cores = 1,
  pb = TRUE,
  min.overlap = 0.5,
  by = NULL,
  solve.ambiguous = TRUE
)
```

## Arguments

- reference:

  Data frame or 'selection.table' (following the warbleR package format)
  with the reference selections (start and end of the sound events) that
  will be used to evaluate the performance of the detection, represented
  by those selections in 'detection'. Must contained at least the
  following columns: "sound.files", "selec", "start" and "end". **It
  must contain the reference selections that will be used for detection
  optimization**.

- detection:

  Data frame or 'selection.table' with the detections (start and end of
  the sound events) that will be compared against the 'reference'
  selections. Must contained at least the following columns:
  "sound.files", "selec", "start" and "end". It can contain data for
  additional sound files not found in 'references'. In this case the
  routine assumes that no sound events are found in those files, so
  detection from those files are all false positives.

- cores:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- min.overlap:

  Numeric. Controls the minimum amount of overlap required for a
  detection and a reference sound for it to be counted as true positive.
  Default is 0.5. Overlap is measured as intersection over union.

- by:

  Character vector with the name of a categorical column in 'detection'
  for running a stratified labeling (e.g. by template). Labels will be
  returned separated for each level in 'by'. Default is `NULL`.

- solve.ambiguous:

  Logical argument to control whether ambiguous detections (i.e. split
  and merged positives) are solved using maximum bipartite graph
  matching. Default is `TRUE`. If `FALSE` ambiguous detections are not
  solved.

## Value

A data frame or selection table (if 'detection' was also a selection
table, warbleR package's format, see
[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.html))
including three additional columns, 'detection.class', which indicates
the class of each detection, 'reference' which identifies the event in
the 'reference' table that was detected and 'overlap' which refers to
the amount of overlap to the reference sound. See
[`diagnose_detection`](https://docs.ropensci.org/ohun/reference/diagnose_detection.md)
for a description of the labels used in 'detection.class'. The output
data frame also contains an additional data frame with the overlap for
each pair of overlapping detection/reference. Overlap is measured as
intersection over union.

## Details

The function identifies the rows in the output of a detection routine as
true or false positives. This is achieved by comparing the data frame to
a reference selection table in which all sound events of interest have
been selected.

## References

Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C.,
Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. (2023). ohun: An R
package for diagnosing and optimizing automatic sound event detection.
Methods in Ecology and Evolution, 14, 2259–2271.
https://doi.org/10.1111/2041-210X.14170

## See also

[`diagnose_detection`](https://docs.ropensci.org/ohun/reference/diagnose_detection.md),
[`summarize_diagnostic`](https://docs.ropensci.org/ohun/reference/summarize_diagnostic.md)

## Author

Marcelo Araya-Salas <marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  # load data
  data("lbh_reference")

  # an extra one in detection (1 false positive)
  label_detection(reference = lbh_reference[-1, ], detection = lbh_reference)

  # missing one in detection (all true positives)
  label_detection(reference = lbh_reference, detection = lbh_reference[-1, ])

  # perfect detection (all true positives)
  label_detection(reference = lbh_reference, detection = lbh_reference)

  # and extra sound file in reference (all true positives)
  label_detection(
    reference = lbh_reference, detection =
      lbh_reference[lbh_reference$sound.files != "lbh1.wav", ]
  )

  # and extra sound file in detection (some false positives)
  label_detection(
    reference =
      lbh_reference[lbh_reference$sound.files != "lbh1.wav", ],
    detection = lbh_reference
  )

  # duplicate 1 detection row (to get 2 splits)
  detec <- lbh_reference[c(1, seq_len(nrow(lbh_reference))), ]
  detec$selec[1] <- 1.2
  label_detection(
    reference = lbh_reference,
    detection = detec
  )

  # merge 2 detections (to get split and merge)
  Y <- lbh_reference
  Y$end[1] <- 1.2
  label_detection(reference = lbh_reference, detection = Y)

  # remove split to get only merge
  Y <- Y[-2, ]
  label_detection(reference = lbh_reference, detection = Y)
}
#> Object of class 'selection_table'
#> * The output of the following call:
#> label_detection(reference = lbh_reference, detection = Y)
#> 
#> Contains: 
#> *  A selection table data frame with 18 rows and 9 columns:
#> sound.files    selec    start      end   bottom.freq   top.freq
#> ------------  ------  -------  -------  ------------  ---------
#> lbh2.wav           1   0.1092   1.2000        2.2954     8.9382
#> lbh2.wav           3   1.2658   1.3856        2.2606     9.0774
#> lbh2.wav           4   1.8697   2.0053        2.1911     8.9035
#> lbh2.wav           5   2.4418   2.5809        2.1563     8.6600
#> lbh2.wav           6   3.0368   3.1689        2.2259     8.9382
#> lbh2.wav           7   3.6286   3.7466        2.3302     8.6252
#> ... 3 more column(s) (detection.class, reference, overlap)
#>  and 12 more row(s)
#> 
#> * A data frame (check.results) with 18 rows generated by check_sels() (as an attribute)
#> created by warbleR < 1.1.21
```
