# Merge overlapping selections

`merge_overlaps` merges several overlapping selections into a single
selection

## Usage

``` r
merge_overlaps(X, pb = TRUE, cores = 1)
```

## Arguments

- X:

  Data frame or 'selection.table' (following the warbleR package format)
  with selections (start and end of the sound events). Must contain at
  least the following columns: "sound.files", "selec", "start" and
  "end".

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- cores:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

## Value

If any time-overlapping selection is found it returns a data frame in
which overlapping selections are collapsed into a single selection.

## Details

The function finds time-overlapping selections in reference tables and
collapses them into a single selection. It can be useful to prepare
reference tables to be used in an energy detection routine. In such
cases overlapping selections are expected to be detected as a single
sound. Therefore, merging them can be useful to prepare references in a
format representing a more realistic expectation of how a perfect energy
detection routine would look like.

## References

Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C.,
Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. (2023). ohun: An R
package for diagnosing and optimizing automatic sound event detection.
Methods in Ecology and Evolution, 14, 2259–2271.
https://doi.org/10.1111/2041-210X.14170

## See also

[`summarize_diagnostic`](https://docs.ropensci.org/ohun/reference/summarize_diagnostic.md),
[`label_detection`](https://docs.ropensci.org/ohun/reference/label_detection.md)

## Author

Marcelo Araya-Salas <marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  # load data
  data("lbh_reference")

  # nothing to merge
  merge_overlaps(lbh_reference)

  # create artificial overlapping selections
  lbh_ref2 <- rbind(as.data.frame(lbh_reference[c(3, 10), ]), lbh_reference[c(3, 10), ])

  lbh_ref2$selec <- seq_len(nrow(lbh_ref2))

  merge_overlaps(lbh_ref2)
}
#> No overlapping selections were found
#> 4 selections overlapped
#>   sound.files selec    start       end bottom.freq top.freq
#> 1    lbh1.wav     2 0.088118 0.2360047      1.9824   8.4861
#> 2    lbh2.wav     1 1.265850 1.3855678      2.2606   9.0774
```
