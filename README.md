# ohun: Automatic detection of acoustic signals


[ohun](https://github.com/maRce10/ohun) is intended to facilitate the automatic detection of acoustic signals, providing functions to diagnose and optimize detection routines. Detections from other software can also be explored and optimized.

 The main features of the package are:
   - The use of reference annotations for detection optimization and diagnostic 
   - The use of signal detection theory diagnostic parameters to evaluate detection performance 
   - The batch processing of sound files for improve computational performance
   
 The package offers functions for:
   - Energy-based detection
   - Template-based detection
   - Diagnose detection precision
   - Optimize detection routines based on reference annotations
   
All functions allow the parallelization of tasks, which distributes the tasks among several processors to improve computational efficiency. The package works on sound files in '.wav', '.mp3', '.flac' and '.wac' format.


To install the latest developmental version from [github](https://github.com/) you will need the R package [devtools](https://cran.r-project.org/package=devtools):

```r
devtools::install_github("maRce10/ohun")

#load package
library(ohun)

```


Please cite [ohun](https://github.com/maRce10/ohun) as follows:

Araya-Salas, M. (2021), ohun: automatic detection of acoustic signals. R package version 0.1.0.

