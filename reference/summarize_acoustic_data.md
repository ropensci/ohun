# Summarize information about file format in an acoustic data set

`summarize_acoustic_data` summarizes information about file format in an
acoustic data set

## Usage

``` r
summarize_acoustic_data(path = ".", digits = 2)
```

## Arguments

- path:

  Character string containing the directory path where the sound files
  are located. Default is `"."` (current working directory).

- digits:

  Numeric vector of length 1 with the number of decimals to include.
  Default is 2.

## Value

The function prints a summary of the format of the files in an acoustic
data set.

## Details

The function summarizes information about file format in an acoustic
data set. It provides information about the number of files, file
formats, sampling rates, bit depths, channels, duration and file size
(in MB). For file format, sampling rate, bit depth and number of
channels the function includes information about the number of files for
each format (e.g. '44.1 kHz (2)' means 2 files with a sampling rate of
44.1 kHz).

## References

Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C.,
Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. (2023). ohun: An R
package for diagnosing and optimizing automatic sound event detection.
Methods in Ecology and Evolution, 14, 2259–2271.
https://doi.org/10.1111/2041-210X.14170

## See also

[`summarize_reference`](https://docs.ropensci.org/ohun/reference/summarize_reference.md)

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
  summarize_acoustic_data(path = tempdir())
}
#> Features of the acoustic data set in '/tmp/RtmpXb5EA8':
#> 
#> * 10 sound files
#> 
#> * 1 file format(s) (.wav (10))
#> 
#> * 1 sampling rate(s) (22.05 kHz (10))
#> 
#> * 1 bit depth(s) (16 bits (10))
#> 
#> * 1 number of channels (1 channel(s) (10))
#> 
#> * File duration range: 0.5-5 s (mean: 2 s)
#> 
#> * File size range: 0.02-0.22 MB (mean: 0.09 MB)
#> 
#>  (detailed information by sound file can be obtained with 'warbleR::info_sound_files()')
```
