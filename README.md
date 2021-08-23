# ohun: Automatic detection of acoustic signals


[ohun](https://github.com/maRce10/ohun) is intended to facilitate the automatic detection of acoustic signals, providing functions to diagnose and optimize detection routines. Detections from other software can also be explored and optimized.

 The main features of the package are:
   \itemize{
   \item The batch processing of sound files for signal detection  
   \item The usage of annotations for detection optimization and diagnostic
   }
   
 The package offers functions to:
   \itemize{
   \item Energy-based detection
   \item Template-based detection
   \item Diagnose detection precision
   \item Optimize detection routines based on reference annotations
   }
   
Most of the functions allow the parallelization of tasks, which distributes the tasks among several processors to improve computational efficiency.


To install the latest developmental version from [github](https://github.com/) you will need the R package [devtools](https://cran.r-project.org/package=devtools):

```r
devtools::install_github("maRce10/warbleR")

#load package
library(warbleR)

```


Please cite [ohun](https://github.com/maRce10/ohun)  as follows:

Araya-Salas, M. (2020), ohun: automatic detection of acoustic signals. R package version 0.1.0.

