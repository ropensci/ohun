# Splits sound files and associated annotations

`split_acoustic_data` splits sound files (and corresponding selection
tables) in shorter clips

## Usage

``` r
split_acoustic_data(
  path = ".",
  sgmt.dur = 10,
  sgmts = NULL,
  files = NULL,
  cores = 1,
  pb = TRUE,
  only.sels = FALSE,
  output.path = file.path(path, "clips"),
  X = NULL,
  overwrite = FALSE
)
```

## Arguments

- path:

  Directory path where sound files are found. The current working
  directory is used as default.

- sgmt.dur:

  Numeric. Duration (in s) of segments in which sound files would be
  split. Sound files shorter than 'sgmt.dur' won't be split. Ignored if
  'sgmts' is supplied.

- sgmts:

  Numeric. Number of segments in which to split each sound file. If
  supplied 'sgmt.dur' is ignored.

- files:

  Character vector indicating the subset of files that will be split.
  Supported file formats:'.wav', '.mp3', '.flac' and '.wac'. If not
  supplied the function will work on all sound files (in the supported
  format) in 'path'.

- cores:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- only.sels:

  Logical argument to control if only the data frame is returned (no
  wave files are saved). Default is `FALSE`.

- output.path:

  Directory path where the output files will be saved. If not supplied
  then a subfolder called 'clips' will be created within the supplied
  'path'.

- X:

  'selection_table' object or a data frame with columns for sound file
  name (sound.files), selection number (selec), and start and end time
  of signal (start and end). If supplied the data frame/selection table
  is modified to reflect the position of the selections in the new sound
  files. Note that some selections could split between 2 segments. To
  deal with this, a 'split.sels' column is added to the data frame in
  which those selections are labeled as 'split'. Default is `NULL`.

- overwrite:

  Logical. If `TRUE` existing files in the output path with the same
  name as the clips being created will be overwritten. Default is
  `FALSE`. This avoids re-creating clips that have already been created
  in previous function calls.

## Value

Wave files for each segment (e.g. clips) in the working directory (if
`only.sels = FALSE`, named as 'sound.file.name-#.wav'). Clips are saved
in .wav format. If 'X' is not supplied the function returns a data frame
containing the name of the original sound files (original.sound.files),
the name of the segments (sound.files) and the start and end of segments
in the original files. If 'X' is supplied then a data frame with the
position of the selections in the newly created clips is returned
instead. However, if 'X' is a 'selection table' and the clips have been
saved, a data frame with the information of the position of clips in the
original sound files is also returned as an attribute in the output
selection table ("clip.info"). Output annotation data contains the
position of the annotations in the new clips, with an additional column,
'split.sels', that inform users whether annotations have been split into
multiple clips ('split') or not (`NA`). For split annotations the
'selec' column will contain the original 'selec' id plus an additional
index (selec-index) so users can still identify from which annotation
splits came from. Sound files in 'path' that are not referenced in 'X'
will still be split. The function may not work properly with very short
segments (\< 1 s).

## Details

This function aims to reduce the size of sound files in order to
simplify some processes that are limited by sound file size (big files
can't be manipulated, e.g.
[`energy_detector`](https://docs.ropensci.org/ohun/reference/energy_detector.md)).

## References

Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C.,
Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. (2023). ohun: An R
package for diagnosing and optimizing automatic sound event detection.
Methods in Ecology and Evolution, 14, 2259–2271.
https://doi.org/10.1111/2041-210X.14170

## See also

[`cut_sels`](https://marce10.github.io/warbleR/reference/cut_sels.html)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
  # load data and save to temporary working directory
  data("lbh1", "lbh2")
  tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
  tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

  # split files in 1 s files
  split_acoustic_data(sgmt.dur = 1, path = tempdir(), 
  files = c("lbh1.wav", "lbh2.wav"))

  # Check this folder
  tempdir()
}
#> [1] "/tmp/RtmpXb5EA8"
```
