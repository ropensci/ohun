params <-
list(EVAL = FALSE)

## ----eval = FALSE-----------------------------------------------------------------------
#  
#  # From CRAN would be
#  install.packages("ohun")
#  
#  #load package
#  library(ohun)
#  

## ----eval = FALSE-----------------------------------------------------------------------
#  
#  # install package
#  remotes::install_github("maRce10/ohun")
#  
#  #load packages
#  library(ohun)
#  library(tuneR)
#  library(warbleR)

## ----global options, echo = FALSE, message=FALSE, warning=FALSE-------------------------

#load packages
library(ohun)
library(tuneR)
library(warbleR)
library(ggplot2)

data("lbh1", "lbh2", "lbh_reference")

# for spectrograms
par(mar = c(5, 4, 2, 2) + 0.1)

stopifnot(require(knitr))
options(width = 90)
opts_chunk$set(
  comment = NA,
  # eval = if (isTRUE(exists("params"))) params$EVAL else FALSE,
  dev = "jpeg",
  dpi = 100,
  fig.width=10,
  out.width = "100%",
  fig.align = "center"
)


## ----eval = TRUE------------------------------------------------------------------------
# load example data
data("lbh1", "lbh2", "lbh_reference")

lbh_reference

## ---------------------------------------------------------------------------------------
# convert to data frame
as.data.frame(lbh_reference)

## ----eval = TRUE, fig.asp=0.4-----------------------------------------------------------
# save sound file
tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))

# save sound file
tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

# print spectrogram
label_spectro(wave = lbh1, reference = lbh_reference[lbh_reference$sound.files == "lbh1.wav", ], hop.size = 10, ovlp = 50, flim = c(1, 10))

# print spectrogram
label_spectro(wave = lbh2, reference = lbh_reference[lbh_reference$sound.files == "lbh2.wav", ], hop.size = 10, ovlp = 50, flim = c(1, 10))

## ---------------------------------------------------------------------------------------
lbh1_reference <-
  lbh_reference[lbh_reference$sound.files == "lbh1.wav",]

# diagnose
diagnose_detection(reference = lbh1_reference, detection = lbh1_reference)[, c(1:3, 7:9)]


## ----fig.asp=0.4------------------------------------------------------------------------
# create new table
lbh1_detection <- lbh1_reference[3:9,]

# print spectrogram
label_spectro(
  wave = lbh1,
  reference = lbh1_reference,
  detection = lbh1_detection,
  hop.size = 10,
  ovlp = 50,
  flim = c(1, 10)
)

# diagnose
diagnose_detection(reference = lbh1_reference, detection = lbh1_detection)[, c(1:3, 7:9)]


## ----fig.asp=0.4------------------------------------------------------------------------
# print spectrogram
label_spectro(
  wave = lbh1,
  detection = lbh1_reference,
  reference = lbh1_detection,
  hop.size = 10,
  ovlp = 50,
  flim = c(1, 10)
)

# diagnose
diagnose_detection(reference = lbh1_detection, detection = lbh1_reference)[, c(1:3, 7:9)]


## ----fig.asp=0.4------------------------------------------------------------------------
# create new table
lbh1_detection <- lbh1_reference

# add 'noise' to start
set.seed(18)
lbh1_detection$start <-
  lbh1_detection$start + rnorm(nrow(lbh1_detection), mean = 0, sd = 0.1)

## print spectrogram
label_spectro(
  wave = lbh1,
  reference = lbh1_reference,
  detection = lbh1_detection,
  hop.size = 10,
  ovlp = 50,
  flim = c(1, 10)
)

# diagnose
diagnose_detection(reference = lbh1_reference, detection = lbh1_detection)


## ---------------------------------------------------------------------------------------
# diagnose with time diagnostics
diagnose_detection(reference = lbh1_reference[-1, ], detection = lbh1_detection[-10, ], time.diagnostics = TRUE)


## ---------------------------------------------------------------------------------------
# diagnose by sound file
diagnostic <-
  diagnose_detection(reference = lbh1_reference,
                     detection = lbh1_detection,
                     by.sound.file = TRUE)

diagnostic

## ---------------------------------------------------------------------------------------
# summarize
summarize_diagnostic(diagnostic)


## ---------------------------------------------------------------------------------------

# ggplot detection and reference
plot_detection(reference = lbh1_reference, detection = lbh1_detection)


## ---------------------------------------------------------------------------------------

# ggplot detection and reference
plot_detection(reference = lbh_reference, detection = lbh_reference)


## ----eval = FALSE, echo=FALSE-----------------------------------------------------------
#  Observaciones:
#  
#  avoid having overlapping selections in reference (check with overlapping_sels())
#  
#  downsample to a freq range just enough for the sound events of interest
#  
#  use hop.size instead of wl
#  
#  after split_acoustic_data() another function that returns the position in the original unsplit sound file
#  
#  count number of detections per unit of time

## ----session info, echo=FALSE-----------------------------------------------------------
sessionInfo()

