% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_diagnostic.R
\name{summarize_diagnostic}
\alias{summarize_diagnostic}
\title{Summarize detection diagnostics}
\usage{
summarize_diagnostic(
  diagnostic,
  time.diagnostics = FALSE,
  macro.average = FALSE
)
}
\arguments{
\item{diagnostic}{A data frame with the output of a detection optimization function (\code{\link{diagnose_detection}}, \code{\link{optimize_energy_detector}} or \code{\link{optimize_template_detector}})}

\item{time.diagnostics}{Logical argument to control if diagnostics related to the duration of the sound events ("mean.duration.true.positives", "mean.duration.false.positives", "mean.duration.false.negatives" and "proportional.duration.true.positives") are returned (if \code{TRUE}). Default is \code{FALSE}.}

\item{macro.average}{Logical argument to control if diagnostics are first calculated for each sound file and then averaged across sound files, which can minimize the effect of unbalanced sample sizes between sound files. If \code{FALSE} (default) diagnostics are based on aggregated statistics irrespective of sound files. The following indices can be estimated by macro-averaging: overlap, mean.duration.true.positives, mean.duration.false.positives, mean.duration.false.positives, mean.duration.false.negatives, proportional.duration.true.positives, recall and precision (f.score is always derived from recall and precision). Note that when applying macro-averaging, recall and precision are not derived from the true positive, false positive and false negative values returned by the function.}
}
\value{
A data frame, similar to the output of a detection optimization function (\code{\link{diagnose_detection}}, \code{\link{optimize_energy_detector}}, \code{\link{optimize_template_detector}}) including the following detection performance diagnostics:
\itemize{
 \item \code{detections}: total number of detections
 \item \code{true.positives}: number of sound events in 'reference' that correspond to any detection. Matching is defined as some degree of overlap in time. In a perfect detection routine it should be equal to the number of rows in 'reference'.
 \item \code{false.positives}: number of detections that don't match (i.e. don't overlap with) any of the sound events in 'reference'. In a perfect detection routine it should be 0.
 \item \code{false.negatives}: number of sound events in 'reference' that were not detected (not found in 'detection'. In a perfect detection routine it should be 0.
 \item \code{splits}: number of detections overlapping reference sounds that also overlap with other detections. In a perfect detection routine it should be 0.
 \item \code{merges}: number of detections that overlap with two or more reference sounds. In a perfect detection routine it should be 0.
 \item \code{mean.duration.true.positives}: mean duration of true positives (in s). Only included when \code{time.diagnostics = TRUE}.
 \item \code{mean.duration.false.positives}: mean duration of false positives (in ms). Only included when \code{time.diagnostics = TRUE}.
 \item \code{mean.duration.false.negatives}: mean duration of false negatives (in ms). Only included when \code{time.diagnostics = TRUE}.
 \item \code{overlap}: mean intersection over union overlap of true positives.
 \item \code{proportional.duration.true.positives}: ratio of duration of true positives to the duration of sound events in 'reference'. In a perfect detection routine it should be 1. Based only on true positives that were not split or merged.
 \item \code{duty.cycle}: proportion of a sound file in which sounds were detected. Only included when \code{time.diagnostics = TRUE} and \code{path} is supplied. Useful when conducting energy-based detection as a perfect detection can be obtained with a very low amplitude threshold, which will detect everything, but will produce a duty cycle close to 1.
 \item \code{recall}: Proportion of sound events in 'reference' that were detected. In a perfect detection routine it should be 1.
 \item \code{precision}: Proportion of detections that correspond to sound events in 'reference'. In a perfect detection routine it should be 1.
 \item \code{f.score}: Combines recall and precision as the harmonic mean of these two. Provides a single value for evaluating performance. In a perfect detection routine it should be 1.
 }
}
\description{
\code{summarize_diagnostic} summarizes detection diagnostics
}
\details{
The function summarizes a detection diagnostic data frame in which diagnostic parameters are shown split by (typically) a categorical column, usually sound files. This function is used internally by \code{\link{diagnose_detection}}. 'splits' and 'merge.positives' are also counted (i.e. counted twice) as 'true.positives'. Therefore "true.positives + false.positives = detections".
}
\examples{
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
}
\references{
Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C., Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. 2022. ohun: an R package for diagnosing and optimizing automatic sound event detection. BioRxiv, 2022.12.13.520253.
Mesaros, A., Heittola, T., & Virtanen, T. (2016). Metrics for polyphonic sound event detection. Applied Sciences, 6(6), 162.
}
\seealso{
\code{\link{diagnose_detection}}
}
\author{
Marcelo Araya-Salas \email{marcelo.araya@ucr.ac.cr})
}
