params <-
list(EVAL = FALSE)

## ----eval = FALSE-------------------------------------------------------------
#  
#  # From CRAN would be
#  install.packages("ohun")
#  
#  #load package
#  library(ohun)
#  

## ----eval = FALSE-------------------------------------------------------------
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
  dpi = 70,
  fig.width=10,
  out.width = "100%",
  fig.align = "center"
)


## ----eval = TRUE------------------------------------------------------------------------
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

## ----eval = TRUE, fig.asp=0.5-----------------------------------------------------------

# print spectrogram
label_spectro(wave = lbh1, reference = lbh1_reference, hop.size = 10, ovlp = 50, flim = c(1, 10), envelope = TRUE)

## ---------------------------------------------------------------------------------------
# install this package first if not installed
# install.packages("Sim.DiffProc")

#Creating vector for duration 
durs <- rep(c(0.3, 1), 5)

#Creating simulated song
set.seed(12)
simulated_1 <-
  warbleR::simulate_songs(
    n = 10,
    durs = durs,
    freqs = 5,
    sig2 = 0.01,
    gaps = 0.5,
    harms = 1,
    bgn = 0.1,
    path = tempdir(),
    file.name = "simulated_1",
    selec.table = TRUE,
    shape = "cos",
    fin = 0.3,
    fout = 0.35,
    samp.rate = 18
  )$wave

## ----fig.asp=0.5------------------------------------------------------------------------
# plot spectrogram and envelope
label_spectro(wave = simulated_1,
              env = TRUE,
              fastdisp = TRUE)


## ----fig.asp=0.5------------------------------------------------------------------------
# run detection
detection <-
  energy_detector(
    files = "simulated_1.wav",
    bp = c(2, 8),
    threshold = 50,
    smooth = 150,
    path = tempdir()
  )

# plot spectrogram and envelope
label_spectro(
  wave = simulated_1,
  envelope = TRUE,
  detection = detection,
  threshold = 50
)

## ---------------------------------------------------------------------------------------
detection

## ----eval = TRUE, echo = TRUE, fig.asp=0.4----------------------------------------------
# run detection
detection <-
  energy_detector(
    files = "simulated_1.wav",
    bp = c(1, 8),
    threshold = 50,
    min.duration = 500,
    smooth = 150,
    path = tempdir()
  )

# plot spectrogram
label_spectro(wave = simulated_1, detection = detection)


## ----eval = TRUE, echo = TRUE, fig.asp=0.4----------------------------------------------
# run detection
detection <- energy_detector(files = "simulated_1.wav", bp = c(1, 8),  threshold = 50, smooth = 150, max.duration = 500, path = tempdir())

# plot spectrogram
label_spectro(wave = simulated_1,  detection = detection)

## ----fig.asp=0.4, eval = TRUE, echo = TRUE----------------------------------------------
# Detecting 
detection <- energy_detector(files = "simulated_1.wav", bp = c(5, 8), threshold = 50, smooth = 150, path = tempdir())

# plot spectrogram
label_spectro(wave = simulated_1,  detection = detection)

## ----fig.asp=0.4, eval = TRUE, echo = TRUE----------------------------------------------
# Detect
detection <-
  energy_detector(
    files = "simulated_1.wav",
    bp = c(0, 6),
    threshold = 50,
    min.duration = 1,
    smooth = 150,
    path = tempdir()
  )

# plot spectrogram
label_spectro(wave = simulated_1,  detection = detection)

## ----eval = TRUE, fig.asp=0.5-----------------------------------------------------------
#Creating simulated song
set.seed(12)

#Creating vector for duration
durs <- rep(c(0.3, 1), 5)

sim_2 <-
  sim_songs(
    n = 10,
    durs = durs,
    freqs = 5,
    sig2 = 0.01,
    gaps = 0.5,
    harms = 1,
    bgn = 0.1,
    path = tempdir(),
    file.name = "simulated_2",
    selec.table = TRUE,
    shape = "cos",
    fin = 0.3,
    fout = 0.35,
    samp.rate = 18,
    am.amps = c(1, 2, 3, 2, 0.1, 2, 3, 3, 2, 1)
  )

# extract wave object and selection table
simulated_2 <- sim_2$wave
sim2_sel_table <- sim_2$selec.table

# plot spectrogram
label_spectro(wave = simulated_2, envelope = TRUE)

## ----eval = TRUE, fig.asp=0.5-----------------------------------------------------------
# detect sounds
detection <- energy_detector(files = "simulated_2.wav", threshold = 50, path = tempdir())

# plot spectrogram
label_spectro(wave = simulated_2, envelope = TRUE, threshold = 50, detection = detection)

## ----eval = TRUE, fig.asp=0.5-----------------------------------------------------------
# detect sounds
detection <-
  energy_detector(
    files = "simulated_2.wav",
    threshold = 50,
    min.duration = 1,
    path = tempdir(),
    hold.time = 200
  )

# plot spectrogram
label_spectro(
  wave = simulated_2,
  envelope = TRUE,
  threshold = 50,
  detection = detection
)

## ----eval = TRUE, fig.asp=0.5-----------------------------------------------------------
# detect sounds
detection <-
  energy_detector(
    files = "simulated_2.wav",
    threshold = 50,
    min.duration = 1,
    path = tempdir(),
    smooth = 350
  )

# plot spectrogram
label_spectro(
  wave = simulated_2,
  envelope = TRUE,
  threshold = 50,
  detection = detection,
  smooth = 350
)

## ---------------------------------------------------------------------------------------
optim_detection <-
  optimize_energy_detector(
    reference = sim2_sel_table,
    files = "simulated_2.wav",
    threshold = 50,
    min.duration = 1,
    path = tempdir(),
    smooth = c(100, 250, 350)
  )

optim_detection[, c(1, 2:5, 7:12, 17:18)]

## ---------------------------------------------------------------------------------------
summarize_reference(reference = lbh_reference, path = tempdir())

## ----session info, echo=FALSE-----------------------------------------------------------
sessionInfo()

