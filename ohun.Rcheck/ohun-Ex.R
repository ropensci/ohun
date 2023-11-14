pkgname <- "ohun"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ohun')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("consensus_detection")
### * consensus_detection

flush(stderr()); flush(stdout())

### Name: consensus_detection
### Title: Remove ambiguous detections
### Aliases: consensus_detection

### ** Examples

{
  # load example data
  data("lbh1", "lbh_reference")

  # save sound files
  tuneR::writeWave(lbh1, file.path(tempdir(), "lbh2.wav"))

  # template for the first sound file in 'lbh_reference'
  templ1 <- lbh_reference[1, ]

  # generate template correlations
  tc <- template_correlator(
    templates = templ1, path = tempdir(),
    files = "lbh2.wav"
  )

  # template detection
  td <- template_detector(template.correlations = tc, threshold = 0.12)

  # this detection generates 2 split positives
  diagnose_detection(
    reference = lbh_reference[lbh_reference == "lbh2.wav", ],
    detection = td
  )

  # label detection
  ltd <- label_detection(
    reference = lbh_reference[lbh_reference == "lbh2.wav", ],
    detection = td
  )

  # now they can be filter to keep the detection with the highest score for each split
  ftd <- consensus_detection(ltd, by = "scores")

  # splits must be 0
  diagnose_detection(
    reference = lbh_reference[lbh_reference == "lbh2.wav", ],
    detection = ftd
  )
}




cleanEx()
nameEx("diagnose_detection")
### * diagnose_detection

flush(stderr()); flush(stdout())

### Name: diagnose_detection
### Title: Evaluate the performance of a sound event detection procedure
### Aliases: diagnose_detection

### ** Examples

{
  # load data
  data("lbh_reference")

  # perfect detection
  diagnose_detection(reference = lbh_reference, detection = lbh_reference)

  # missing one in detection
  diagnose_detection(reference = lbh_reference, detection = lbh_reference[-1, ])

  # an extra one in detection
  diagnose_detection(reference = lbh_reference[-1, ], detection = lbh_reference)

  # with time diagnostics
  diagnose_detection(
    reference = lbh_reference[-1, ],
    detection = lbh_reference, time.diagnostics = TRUE
  )

  # and extra sound file in reference
  diagnose_detection(
    reference = lbh_reference,
    detection =
      lbh_reference[lbh_reference$sound.files != "lbh1", ]
  )

  # and extra sound file in detection
  diagnose_detection(
    reference =
      lbh_reference[lbh_reference$sound.files != "lbh1", ],
    detection = lbh_reference
  )

  # and extra sound file in detection by sound file
  dd <- diagnose_detection(
    reference =
      lbh_reference[lbh_reference$sound.files != "lbh1", ],
    detection = lbh_reference, time.diagnostics = TRUE, by.sound.file = TRUE
  )

  # get summary
  summarize_diagnostic(dd)
}



cleanEx()
nameEx("energy_detector")
### * energy_detector

flush(stderr()); flush(stdout())

### Name: energy_detector
### Title: Detects the start and end of sound events
### Aliases: energy_detector

### ** Examples





cleanEx()
nameEx("get_envelopes")
### * get_envelopes

flush(stderr()); flush(stdout())

### Name: get_envelopes
### Title: Extract absolute amplitude envelopes
### Aliases: get_envelopes

### ** Examples

{
  # Save to temporary working directory
  data(list = c("lbh1", "lbh2"))
  tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
  tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

  # get raw absolute amplitude envelopes
  envs <- get_envelopes(path = tempdir())

  # extract segment for the first sound event in the first sound file
  x <- envs[[1]]$envelope

  # and plot it
  plot(x[(length(x) / 9):(length(x) / 4)], type = "l", xlab = "samples", ylab = "amplitude")

  # smoothing envelopes
  envs <- get_envelopes(path = tempdir(), smooth = 6.8)
  x <- envs[[1]]$envelope
  plot(x[(length(x) / 9):(length(x) / 4)], type = "l", xlab = "samples", ylab = "amplitude")

  # smoothing and thinning
  envs <- get_envelopes(path = tempdir(), thinning = 1 / 10, smooth = 6.8)
  x <- envs[[1]]$envelope
  plot(x[(length(x) / 9):(length(x) / 4)], type = "l", xlab = "samples", ylab = "amplitude")

  # no normalization
  envs <- get_envelopes(path = tempdir(), thinning = 1 / 10, smooth = 6.8)
  x <- envs[[1]]$envelope
  plot(x[(length(x) / 9):(length(x) / 4)],
    type = "l", xlab = "samples", ylab = "amplitude",
    normalize = FALSE
  )
}




cleanEx()
nameEx("get_templates")
### * get_templates

flush(stderr()); flush(stdout())

### Name: get_templates
### Title: Find templates representative of the structural variation of
###   sound events
### Aliases: get_templates

### ** Examples

{
  # Save example files into temporary working directory
  data("lbh1", "lbh2", "lbh_reference")
  tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
  tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

  # get a single mean template
  template <- get_templates(reference = lbh_reference, path = tempdir())

  # get 3 templates
  template <- get_templates(reference = lbh_reference, n.sub.spaces = 3, path = tempdir())
}




cleanEx()
nameEx("label_detection")
### * label_detection

flush(stderr()); flush(stdout())

### Name: label_detection
### Title: Label detections from a sound event detection procedure
### Aliases: label_detection

### ** Examples

{
  # load data
  data("lbh_reference")

  # an extra one in detection (1 false positive)
  label_detection(reference = lbh_reference[-1, ], detection = lbh_reference)

  # missing one in detection (all true positives)
  label_detection(reference = lbh_reference, detection = lbh_reference[-1, ])

  # perfect detection (all true positives)
  label_detection(reference = lbh_reference, detection = lbh_reference)

  # and extra sound file in reference (all true positives)
  label_detection(
    reference = lbh_reference, detection =
      lbh_reference[lbh_reference$sound.files != "lbh1.wav", ]
  )

  # and extra sound file in detection (some false positives)
  label_detection(
    reference =
      lbh_reference[lbh_reference$sound.files != "lbh1.wav", ],
    detection = lbh_reference
  )

  # duplicate 1 detection row (to get 2 splits)
  detec <- lbh_reference[c(1, seq_len(nrow(lbh_reference))), ]
  detec$selec[1] <- 1.2
  label_detection(
    reference = lbh_reference,
    detection = detec
  )

  # merge 2 detections (to get split and merge)
  Y <- lbh_reference
  Y$end[1] <- 1.2
  label_detection(reference = lbh_reference, detection = Y)

  # remove split to get only merge
  Y <- Y[-2, ]
  label_detection(reference = lbh_reference, detection = Y)
}



cleanEx()
nameEx("label_spectro")
### * label_spectro

flush(stderr()); flush(stdout())

### Name: label_spectro
### Title: Plot a labeled spectrogram
### Aliases: label_spectro

### ** Examples

{
  # load example data
  data(list = "lbh1", "lbh_reference")

  # adding labels
  label_spectro(
    wave = lbh1,
    reference = lbh_reference[lbh_reference$sound.files == "lbh1.wav", ],
    wl = 200, ovlp = 50, flim = c(1, 10)
  )

  # adding envelope
  label_spectro(
    wave = lbh1,
    detection = lbh_reference[lbh_reference$sound.files == "lbh1.wav", ],
    wl = 200, ovlp = 50, flim = c(1, 10)
  )

  # see the package vignette for more examples
}




cleanEx()
nameEx("merge_overlaps")
### * merge_overlaps

flush(stderr()); flush(stdout())

### Name: merge_overlaps
### Title: Merge overlapping selections
### Aliases: merge_overlaps

### ** Examples

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



cleanEx()
nameEx("optimize_energy_detector")
### * optimize_energy_detector

flush(stderr()); flush(stdout())

### Name: optimize_energy_detector
### Title: Optimize energy-based sound event detection
### Aliases: optimize_energy_detector

### ** Examples





cleanEx()
nameEx("optimize_template_detector")
### * optimize_template_detector

flush(stderr()); flush(stdout())

### Name: optimize_template_detector
### Title: Optimize acoustic template detection
### Aliases: optimize_template_detector

### ** Examples

{
# Save sound files to temporary working directory
data("lbh1", "lbh2", "lbh_reference")
tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

# template for the second sound file in 'lbh_reference'
templ <- lbh_reference[11, ]

# generate template correlations
tc <- template_correlator(templates = templ, path = tempdir(),
files = "lbh2.wav")

# using 2 threshold
optimize_template_detector(template.correlations = tc, reference =
lbh_reference[lbh_reference$sound.files == "lbh2.wav", ],
threshold = c(0.2, 0.5))

# using several thresholds
optimize_template_detector(template.correlations = tc,
reference = lbh_reference[lbh_reference$sound.files == "lbh2.wav", ],
 threshold = seq(0.5, 0.9, by = 0.05))

 # template for the first and second sound file in 'lbh_reference'
 templ <- lbh_reference[c(1, 11), ]

 # generate template correlations
 tc <- template_correlator(templates = templ, path = tempdir(),
 files = c("lbh1.wav", "lbh2.wav"))

optimize_template_detector(template.correlations = tc, reference =
  lbh_reference, threshold = seq(0.5, 0.7, by = 0.1))

 # showing diagnostics by sound file
 optimize_template_detector(template.correlations = tc, reference =
 lbh_reference,
 threshold = seq(0.5, 0.7, by = 0.1), by.sound.file = TRUE)
}




cleanEx()
nameEx("plot_detection")
### * plot_detection

flush(stderr()); flush(stdout())

### Name: plot_detection
### Title: Plot detection and reference annotations
### Aliases: plot_detection

### ** Examples

{
  # load data
  data("lbh_reference")

  # mid point and regular size
  plot_detection(
    reference = lbh_reference[-14, ],
    detection = lbh_reference[-1, ], mid.point = TRUE
  )

  # mid point and larger size
  plot_detection(
    reference = lbh_reference[-14, ],
    detection = lbh_reference[-1, ], mid.point = TRUE, size = 25
  )

  # true time rectangles
  plot_detection(
    reference = lbh_reference[-14, ],
    detection = lbh_reference[-1, ]
  )

  # use position to make reference and anotations overlap vertically
  plot_detection(
    reference = lbh_reference[-14, ],
    detection = lbh_reference[-1, ], positions = c(1, 1.4)
  )

  # modified using ggplot
  gg_pd <- plot_detection(
    reference = lbh_reference[-14, ],
    detection = lbh_reference[-1, ], positions = c(1, 1.4)
  )

  gg_pd + ggplot2::theme_classic(base_size = 25)
}



cleanEx()
nameEx("split_acoustic_data")
### * split_acoustic_data

flush(stderr()); flush(stdout())

### Name: split_acoustic_data
### Title: Splits sound files and associated annotations
### Aliases: split_acoustic_data

### ** Examples

{
  # load data and save to temporary working directory
  data("lbh1", "lbh2")
  tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
  tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

  # split files in 1 s files
  split_acoustic_data(sgmt.dur = 1, path = tempdir())

  # Check this folder
  tempdir()
}




cleanEx()
nameEx("summarize_acoustic_data")
### * summarize_acoustic_data

flush(stderr()); flush(stdout())

### Name: summarize_acoustic_data
### Title: Summarize information about file format in an acoustic data set
### Aliases: summarize_acoustic_data

### ** Examples

{
  # load data and save example files into temporary working directory
  data("lbh1", "lbh2", "lbh_reference")
  tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
  tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

  # summary across sound files
  summarize_acoustic_data(path = tempdir())
}



cleanEx()
nameEx("summarize_diagnostic")
### * summarize_diagnostic

flush(stderr()); flush(stdout())

### Name: summarize_diagnostic
### Title: Summarize detection diagnostics
### Aliases: summarize_diagnostic

### ** Examples

{
  # load example selection tables

  data("lbh_reference")

  # run diagnose_detection() by sound file
  diag <- diagnose_detection(
    reference = lbh_reference,
    detection = lbh_reference[-1, ], by.sound.file = TRUE
  )

  # summarize
  summarize_diagnostic(diagnostic = diag)

  # should be the same as this:
  diagnose_detection(
    reference = lbh_reference,
    detection = lbh_reference[-1, ], by.sound.file = FALSE
  )
}



cleanEx()
nameEx("summarize_reference")
### * summarize_reference

flush(stderr()); flush(stdout())

### Name: summarize_reference
### Title: Summarize temporal and frequency dimensions of annotations and
###   gaps
### Aliases: summarize_reference

### ** Examples

{
  # load data and save example files into temporary working directory
  data("lbh1", "lbh2", "lbh_reference")
  tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
  tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

  # summary across sound files
  summarize_reference(reference = lbh_reference, path = tempdir())

  # summary across sound files
  summarize_reference(reference = lbh_reference, by.sound.file = TRUE, path = tempdir())
}



cleanEx()
nameEx("template_correlator")
### * template_correlator

flush(stderr()); flush(stdout())

### Name: template_correlator
### Title: Acoustic templates correlator using time-frequency
###   cross-correlation
### Aliases: template_correlator

### ** Examples

{
  # load example data
  data("lbh1", "lbh2", "lbh_reference")

  # save sound files
  tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
  tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

  # create template
  templ <- lbh_reference[4, ]
  templ2 <- warbleR::selection_table(templ,
    extended = TRUE, confirm.extended = FALSE,
    path = tempdir()
  )

  # fourier spectrogram
  (tc_fr <- template_correlator(templates = templ, path = tempdir(), type = "fourier"))

  # mel auditory spectrograms
  (tc_ma <- template_correlator(templates = templ, path = tempdir(), type = "mel-auditory"))

  # mfcc spectrograms
  (tc_mfcc <- template_correlator(templates = templ, path = tempdir(), type = "mfcc"))

  # similar results (but no exactly the same) are found with the 3 methods
  # these are the correlation of the correlation vectors
  # fourier vs mel-auditory
  cor(
    tc_fr$`lbh2.wav-4/lbh2.wav`$correlation.scores,
    tc_ma$`lbh2.wav-4/lbh2.wav`$correlation.scores
  )

  # fourier vs mfcc
  cor(
    tc_fr$`lbh2.wav-4/lbh2.wav`$correlation.scores,
    tc_mfcc$`lbh2.wav-4/lbh2.wav`$correlation.scores
  )

  # mel-auditory vs mfcc
  cor(
    tc_ma$`lbh2.wav-4/lbh2.wav`$correlation.scores,
    tc_mfcc$`lbh2.wav-4/lbh2.wav`$correlation.scores
  )

  # using an extended selection table
  templ_est <- warbleR::selection_table(templ,
    extended = TRUE, confirm.extended = FALSE,
    path = tempdir()
  )

  tc_fr_est <- template_correlator(templates = templ_est, path = tempdir(), type = "fourier")

  # produces the same result as templates in a regular data frame
  cor(
    tc_fr$`lbh2.wav-4/lbh2.wav`$correlation.scores,
    tc_fr_est$`lbh2.wav_4-1/lbh2.wav`$correlation.scores
  )
}



cleanEx()
nameEx("template_detector")
### * template_detector

flush(stderr()); flush(stdout())

### Name: template_detector
### Title: Acoustic template detection from time-frequency
###   cross-correlations
### Aliases: template_detector

### ** Examples

{
  # load example data
  data("lbh1", "lbh2", "lbh_reference")

  # save sound files
  tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
  tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

  # template for the first sound file in 'lbh_reference'
  templ1 <- lbh_reference[1, ]

  # generate template correlations
  tc <- template_correlator(templates = templ1, path = tempdir(), files = "lbh1.wav")

  # template detection
  td <- template_detector(template.correlations = tc, threshold = 0.4)

  # diagnose detection
  diagnose_detection(
    reference =
      lbh_reference[lbh_reference$sound.files == "lbh1.wav", ],
    detection = td
  )

  # template for the second and third sound file in 'lbh_reference'
  # which have similar song types
  templ2 <- lbh_reference[4, ]

  # generate template correlations
  tc <- template_correlator(
    templates = templ2, path = tempdir(),
    files = c("lbh1.wav", "lbh2.wav")
  )

  # template detection
  td <- template_detector(template.correlations = tc, threshold = 0.3)

  # diagnose detection
  diagnose_detection(reference = lbh_reference, detection = td)
}



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
