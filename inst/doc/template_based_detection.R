params <-
list(EVAL = FALSE)

## ---- eval = FALSE----------------------------------------------------------------------
#  
#  # From CRAN would be
#  install.packages("ohun")
#  
#  #load package
#  library(ohun)
#  

## ---- eval = FALSE----------------------------------------------------------------------
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

load("../data/lbh2.rda")
load("../data/lbh1.rda")
load("../data/lbh_reference.rda")

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


## ---- eval = TRUE-----------------------------------------------------------------------
# load example data
data("lbh1", "lbh2", "lbh_reference")

# save sound files
tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

# select a subset of the data
lbh1_reference <-
  lbh_reference[lbh_reference$sound.files == "lbh1.wav",]

# print data
lbh1_reference

## ---- eval = TRUE, fig.asp=0.4----------------------------------------------------------

# print spectrogram
label_spectro(wave = lbh1, reference = lbh1_reference, hop.size = 10, ovlp = 50, flim = c(1, 10))

## ---- eval = FALSE, echo = TRUE---------------------------------------------------------
#  # get mean structure template
#  template <-
#    get_templates(reference = lbh1_reference, path = tempdir())

## ---- fig.asp=0.7, out.width="80%", eval = TRUE, echo = FALSE---------------------------
par(mar = c(5, 4, 1, 1), bg = "white")

# get mean structure template
template <-
  get_templates(reference = lbh1_reference, path = tempdir())

## ---- eval = FALSE, echo = TRUE---------------------------------------------------------
#  # get 3 templates
#  get_templates(reference = lbh1_reference,
#                            n.sub.spaces = 3, path = tempdir())

## ---- fig.asp=0.7, out.width="80%", eval = TRUE, echo = FALSE---------------------------
par(mar = c(5, 4, 1, 1), bg = "white")

# get 3 templates
templates <- get_templates(reference = lbh1_reference, 
                          n.sub.spaces = 3, path = tempdir())

## ---------------------------------------------------------------------------------------
# get correlations
correlations <-
  template_correlator(templates = template,
                      files = "lbh1.wav",
                      path = tempdir())

## ---------------------------------------------------------------------------------------
# print
correlations

## ---------------------------------------------------------------------------------------
# run detection
detection <-
  template_detector(template.correlations = correlations, threshold = 0.7)

detection

## ---- fig.asp=0.5-----------------------------------------------------------------------
# plot spectrogram
label_spectro(
  wave = lbh1,
  detection = detection,
  template.correlation = correlations[[1]],
  flim = c(0, 10),
  threshold = 0.4,
  hop.size = 10, ovlp = 50)


## ---------------------------------------------------------------------------------------
#diagnose
diagnose_detection(reference = lbh1_reference, detection = detection)


## ---------------------------------------------------------------------------------------
# run optimization
optimization <-
  optimize_template_detector(
    template.correlations = correlations,
    reference = lbh1_reference,
    threshold = seq(0.1, 0.5, 0.1)
  )

# print output
optimization

## ---------------------------------------------------------------------------------------
# run optimization
optimize_template_detector(
  template.correlations = correlations,
  reference = lbh1_reference,
  threshold = c(0.6, 0.7),
  previous.output = optimization
)

## ---------------------------------------------------------------------------------------

# get correlations
correlations <-
  template_correlator(
    templates = lbh1_reference[c(1, 10),],
    files = "lbh1.wav",
    path = tempdir()
  )

# run detection
detection <-
  template_detector(template.correlations = correlations, threshold = 0.6)



## ---------------------------------------------------------------------------------------

#diagnose
diagnose_detection(reference = lbh1_reference, detection = detection)


## ---------------------------------------------------------------------------------------

#diagnose
diagnose_detection(reference = lbh1_reference, detection = detection, by = "template")


## ---------------------------------------------------------------------------------------
# labeling detection
labeled <-
  label_detection(reference = lbh_reference, detection = detection, by = "template")


## ---------------------------------------------------------------------------------------

# filter
consensus <- consensus_detection(detection = labeled, by = "scores")

# diagnose
diagnose_detection(reference = lbh1_reference, detection = consensus)


## ---- eval = FALSE, echo=FALSE----------------------------------------------------------
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

