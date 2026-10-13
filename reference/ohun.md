# ohun: Optimizing sound event detection

ohun is intended to facilitate the automated detection of sound events,
providing functions to diagnose and optimize detection routines.
Detections from other software can also be explored and optimized.

## Details

The main features of the package are:

- The use of reference annotations for detection optimization and
  diagnosis

- The use of signal detection theory diagnostic parameters to evaluate
  detection performance

- The batch processing of sound files to improve computational
  performance

The package offers functions for:

- Energy-based detection

- Template-based detection

- Diagnose detection precision

- Improve detection by adjusting parameters to optimize accuracy

All functions allow the parallelization of tasks, which distributes the
tasks among several processors to improve computational efficiency. The
package works on sound files in '.wav', '.mp3', '.flac' and '.wac'
format.

License: GPL (\>= 2)

## See also

Useful links:

- <https://docs.ropensci.org/ohun/>

- <https://github.com/ropensci/ohun/>

- Report bugs at <https://github.com/ropensci/ohun/issues/>

## Author

Marcelo Araya-Salas

Maintainer: Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)
