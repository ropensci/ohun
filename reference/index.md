# Package index

## Prepare data for detection

Prepare annotations and acoustic data for automated detection

- [`split_acoustic_data()`](https://docs.ropensci.org/ohun/reference/split_acoustic_data.md)
  : Splits sound files and associated annotations
- [`get_envelopes()`](https://docs.ropensci.org/ohun/reference/get_envelopes.md)
  : Extract absolute amplitude envelopes
- [`envelopes`](https://docs.ropensci.org/ohun/reference/envelopes.md) :
  Class 'envelopes': list of absolute amplitude envelopes
- [`get_templates()`](https://docs.ropensci.org/ohun/reference/get_templates.md)
  : Find templates representative of the structural variation of sound
  events
- [`summarize_reference()`](https://docs.ropensci.org/ohun/reference/summarize_reference.md)
  : Summarize temporal and frequency dimensions of annotations and gaps
- [`summarize_acoustic_data()`](https://docs.ropensci.org/ohun/reference/summarize_acoustic_data.md)
  : Summarize information about file format in an acoustic data set

## Automated detection

Functions implementing automated sound event detection

- [`energy_detector()`](https://docs.ropensci.org/ohun/reference/energy_detector.md)
  : Detects the start and end of sound events
- [`optimize_energy_detector()`](https://docs.ropensci.org/ohun/reference/optimize_energy_detector.md)
  : Optimize energy-based sound event detection
- [`template_correlator()`](https://docs.ropensci.org/ohun/reference/template_correlator.md)
  : Acoustic templates correlator using time-frequency cross-correlation
- [`template_detector()`](https://docs.ropensci.org/ohun/reference/template_detector.md)
  : Acoustic template detection from time-frequency cross-correlations
- [`optimize_template_detector()`](https://docs.ropensci.org/ohun/reference/optimize_template_detector.md)
  : Optimize acoustic template detection

## Curating detection datasets

Exploration and manipulation of the output of detection functions

- [`consensus_detection()`](https://docs.ropensci.org/ohun/reference/consensus_detection.md)
  : Remove ambiguous detections
- [`merge_overlaps()`](https://docs.ropensci.org/ohun/reference/merge_overlaps.md)
  : Merge overlapping selections
- [`summarize_diagnostic()`](https://docs.ropensci.org/ohun/reference/summarize_diagnostic.md)
  : Summarize detection diagnostics
- [`label_detection()`](https://docs.ropensci.org/ohun/reference/label_detection.md)
  : Label detections from a sound event detection procedure
- [`diagnose_detection()`](https://docs.ropensci.org/ohun/reference/diagnose_detection.md)
  : Evaluate the performance of a sound event detection procedure
- [`reassemble_detection()`](https://docs.ropensci.org/ohun/reference/reassemble_detection.md)
  : Reassemble detections from clips

## Built in datasets

Datasets included in ohun

- [`lbh1`](https://docs.ropensci.org/ohun/reference/lbh1.md) :
  Long-billed hermit recording
- [`lbh2`](https://docs.ropensci.org/ohun/reference/lbh2.md) :
  Long-billed hermit recording
- [`lbh_reference`](https://docs.ropensci.org/ohun/reference/lbh_reference.md)
  : Example data frame of a selection table including all sound events
  of interests

## Additional functions

- [`label_spectro()`](https://docs.ropensci.org/ohun/reference/label_spectro.md)
  : Plot a labeled spectrogram
- [`plot_detection()`](https://docs.ropensci.org/ohun/reference/plot_detection.md)
  : Plot detection and reference annotations
