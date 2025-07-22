<!-- based on: https://devguide.ropensci.org/newstemplate.html#newstemplate -->

ohun 1.0.3
=========================

### NEW FEATURES

  * New function `reassemble_detection()` to reassembles detections made on clips so they refer to the original sound files

### MINOR IMPROVEMENTS

  * Update names of functions from package warbleR those in latest versions
  * Replace `sapply()` with `vapply()`
  * Fix bug when true positive are set to false positives based on solving ambiguous detection using maximum bipartite graph matching 

ohun 1.0.2
=========================
Update requested by CRAN.

ohun 1.0.1
=========================

Update requested by CRAN.

ohun 1.0.0 
=========================

### NEW FEATURES

  * New function `plot_detection()` to visually inspect detections 

### MINOR IMPROVEMENTS

  * `sp` package replaced by `sf`
  * Replace `sapply()` with `vapply()`
  * performance indices name changes: split.positives to splits, merged.positives to merges, proportional.overlap.to.true.positives to overlap and f1.score to f.score 
  * `label_detection()` renamed `consensus_detection()`
  

ohun 0.1.0 (2022-12-19)
=========================

### NEW FEATURES

  * released to CRAN
