#' @title Optimize energy-based signal detection
#'
#' @description Optimize energy-based signal detection under different correlation treshold values
#' @usage optimize_energy_detector(reference, files = NULL, threshold = 15, power = 1,
#'  wl = 512, ssmooth = 0, hold.time = 0, min.duration = NULL, max.duration = NULL,
#'   thinning = 1, filter = "ffilter", parallel = 1, pb = TRUE, by.sound.file = FALSE,
#'   bp = NULL, path = NULL, previous.output = NULL)
#' @param reference 'selection_table' object or a data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end). \strong{It should contain the reference selections that will be used for detection optimization}.
#' @param files Character vector indicating the sound files that will be analyzed. Optional. If  not supplied the function will work on the sound files in 'reference'. It can be used to include signals with no signals.
#' @param threshold A numeric vector specifying the amplitude threshold for detecting
#'   signals (in \%). Default is 15. \strong{Several values can be supplied for optimization}.
#' @param power A numeric vector indicating a power factor applied to the amplitude envelope. Increasing power will reduce low amplitude modulations and increase high amplitude modulations, in order to reduce background noise. Default is 1 (no change). \strong{Several values can be supplied for optimization}.
#' @param wl A numeric vector of length 1 specifying the window used internally by
#' \code{\link[seewave]{ffilter}} for bandpass filtering (so only applied when 'bp' is supplied). Default is 512.
#' @param ssmooth A numeric vector to smooth the amplitude envelope
#'   with a sum smooth function. Default is 0 (no smoothing). \strong{Several values can be supplied for optimization}.
#' @param hold.time Numeric vector of length 1. Specifies the time range at which selections will be merged (i.e. if 2 selections are separated by less than the specified hold.time they will be merged in to a single selection). Default is \code{0} (no hold time applied). \strong{Several values can be supplied for optimization}.
#' @param min.duration Numeric vector giving the shortest duration (in
#'   seconds) of the signals to be detected. It removes signals below that
#'   threshold. \strong{Several values can be supplied for optimization}.
#' @param max.duration Numeric vector giving the longest duration (in
#'   seconds) of the signals to be detected. It removes signals above that
#'   threshold. \strong{Several values can be supplied for optimization}.
#' @param thinning Numeric vector in the range 0~1 indicating the proportional reduction of the number of
#' samples used to represent amplitude envelopes (i.e. the thinning of the envelopes). Usually amplitude envelopes have many more samples
#' than those needed to accurately represent amplitude variation in time, which affects the size of the
#' output (usually very large R objects / files). Default is  \code{1} (no thinning). Higher sampling rates may afford higher size reduction (e.g. lower thinning values). Reduction is conducted by interpolation using \code{\link[stats]{approx}}. Note that thinning may decrease time precision, and the higher the thinning the less precise the time detection. \strong{Several values can be supplied for optimization}.
#' @param filter Character vector of length 1 indicating the bandpass filter to be applied (only used if 'bp' is supplied). Three options available, (corresponding to the frequency filter functions in the 'seewave' package): ffilter (\code{\link[seewave]{ffilter}}), bwfilter (\code{\link[seewave]{bwfilter}}) and fir (\code{\link[seewave]{fir}}). \strong{Several values can be supplied for optimization}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @param by.sound.file Logical argument to control whether performance diagnostics are summarized across sound files (when \code{by.sound.file = FALSE}, when more than 1 sound file is included in 'reference') or shown separated by sound file. Default is \code{FALSE}.
#' @param bp Numeric vector of length 2 giving the lower and upper limits of a
#'   frequency bandpass filter (in kHz). Default is \code{NULL}.  This argument is used internally by \code{\link{get_envelopes}}. Not used if 'envelopes' are supplied.
#' @param path Character string containing the directory path where the sound files are located.
#' If \code{NULL} (default) then the current working directory is used.
#' @param previous.output Data frame with the output of a previous run of this function. This will be used to include previous results in the new output and avoid recalculating detection performance for parameter combinations previously evaluated.
#' @return A data frame in which each row shows the result of a detection job with a particular combination of tuning parameters (including in the data frame). It also includes the following diagnostic metrics:
#' \itemize{
#'  \item \code{true.positives}: number of detections that correspond to signals in 'reference'. Matching is defined as some degree of overlap in time. In a perfect detection routine it should be equal to the number of rows in 'reference'.
#'  \item \code{false.positives}: number of detections that don't match any of the signals in 'reference'. In a perfect detection routine it should be 0.
#'  \item \code{false.negatives}: number of signals in 'reference' that were not detected (not found in 'detection'. In a perfect detection routine it should be 0.
#'  \item \code{split.positives}: number of signals in 'reference' that were overlapped by more than 1 detection (i.e. detections that were split). In a perfect detection routine it should be 0.
#'  \item \code{merged.positives}: number of signals in 'detection' that were overlapped by more than 1 detection (i.e. signals that were merged). In a perfect detection routine it should be 0.
#'  \item \code{mean.duration.true.positives}: mean duration of true positives (in s). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{mean.duration.false.positives}: mean duration of false positives (in s). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{mean.duration.false.negatives}: mean duration of false negatives (in s). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{proportional.duration.true.positives}: ratio of total duration of true positives to the total duration of signals in 'reference'. In a perfect detection routine it should be 1. Based only on true positives with that were not split or merged. Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{sensitivity}: Proportion of signals in 'reference' that were detected. In a perfect detection routine it should be 1.
#'  \item \code{specificity}: Proportion of detections that correspond to signals in 'reference' that were detected. In a perfect detection routine it should be 1.
#'  }
##' @export
#' @name optimize_energy_detector
#' @details This function takes a selections data frame or 'selection_table' ('reference')  estimates the detection performance of a energy detector under different detection parameter combinations. This is done by comparing the position in time of the detection to those of the reference selections in 'reference'. The function returns several diagnostic metrics to allow user to determine which parameter values provide a detection that more closely matches the selections in 'reference'. Those parameters can be later used for performing a more efficient detection using \code{\link{energy_detector}}.
#'
#' @examples{
#' # Save example files into temporary working directory
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_reference"))
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"))
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"))
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"))
#'
#' # using smoothing and minimum duration
#' optimize_energy_detector(reference = lbh_selec_reference, path = tempdir(),
#' threshold = c(6, 10), ssmooth = 300, bp = c(2, 9), wl = 300, min.duration = 0.09)
#'
#' #  2 different filters
#' optimize_energy_detector(reference = lbh_selec_reference, path = tempdir(),
#' threshold = c(6, 10, 15), ssmooth = 300, filter = c("ffilter", "fir"),
#' bp = c(2, 9), wl = 300, min.duration = 0.09)
#'
#' # with thinning and smoothing
#' optimize_energy_detector(reference = lbh_selec_reference, path = tempdir(),
#'  threshold = c(6, 10, 15), ssmooth = c(300, 1000), thinning = c(0.1, 0.01),
#'  bp = c(2, 9), wl = 300, min.duration = 0.09)
#'
#' # by sound file
#' (opt_ed <- optimize_energy_detector(reference = lbh_selec_reference, path = tempdir(),
#' threshold = c(6, 10, 15), ssmooth = 300, filter = c("ffilter", "fir"),
#' bp = c(2, 9), wl = 300, min.duration = 0.09, by.sound.file = TRUE))
#'
#' # summarize
#' summarize_diagnostic(opt_ed)
#'
#' # using hold time
#' (op_ed <- optimize_energy_detector(reference = lbh_selec_reference, threshold = 10,
#' hold.time = c(0.1, 0.15), bp = c(2, 9), wl = 300, path = tempdir()))
#'
#' # including previous output in new call
#' optimize_energy_detector(reference = lbh_selec_reference, threshold = 10,
#' hold.time = c(0.05, 0.2), previous.output = op_ed,
#' bp = c(2, 9), wl = 300, path = tempdir())
#'
#' # having and extra file in files (simulating a file that should have no detetions)
#' sub_reference <- lbh_selec_reference[lbh_selec_reference$sound.files != "Phae.long1.wav", ]
#'
#' optimize_energy_detector(reference = sub_reference, files = unique(lbh_selec_reference$sound.files),
#' threshold = 10, hold.time = c(0.1, 0.15), bp = c(2, 9), wl = 300, path = tempdir())
#' }
#'
#' @references {
#' Araya-Salas, M. (2021), ohun: automatic detection of acoustic signals. R package version 0.1.0.
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}).
#last modification on dec-21-2021 (MAS)

optimize_energy_detector <- function(reference, files = NULL, threshold = 15, power = 1, wl = 512, ssmooth = 0, hold.time = 0, min.duration = NULL, max.duration = NULL, thinning = 1, filter = "ffilter", parallel = 1, pb = TRUE, by.sound.file = FALSE, bp = NULL, path = NULL, previous.output = NULL){

  if (is_extended_selection_table(reference)) stop("This function cannot take extended selection tables ('reference' argument)")

  #if reference is not a data frame
  if (!any(is.data.frame(reference), is_selection_table(reference)))
    stop("reference is not of a class 'data.frame' or 'selection_table'")

  #check if all columns are found
  if (any(!(c(
    "sound.files", "selec", "start", "end"
  ) %in% colnames(reference))))
    stop(paste(paste(
      c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec",
                                                    "start", "end") %in% colnames(reference))], collapse =
        ", "
    ), "column(s) not found in 'reference'"))

  #if there are NAs in start or end stop
  if (any(is.na(c(reference$end, reference$start))))
    stop("NAs found in start and/or end columns")

  #if end or start are not numeric stop
  if (any(!is(reference$end, "numeric"),!is(reference$start, "numeric")))
    stop("'start' and 'end' must be numeric")

  #if any start higher than end stop
  if (any(reference$end - reference$start <= 0))
    stop(paste(
      "Start is higher than or equal to end in",
      length(which(reference$end - reference$start <= 0)),
      "case(s)"
    ))

  #check path to working directory
  if (is.null(path)) path <- getwd() else
    if (!dir.exists(path)) stop("'path' provided does not exist") else
      path <- normalizePath(path)

    # if files not supplied then used those from reference
    if (is.null(files))
      files <- unique(reference$sound.files)

      # get all possible combinations of parameters
      exp_grd <- expand.grid(threshold = threshold, power = power, ssmooth = ssmooth, hold.time = hold.time, min.duration = if(is.null(min.duration)) -Inf else min.duration, max.duration = if(is.null(max.duration)) Inf else max.duration, thinning = thinning, filter = filter)

      # if previous output included
      if (!is.null(previous.output)){

        # create composed variable to find overlapping runs
        previous.output$temp.label <- apply(previous.output[, c("threshold", "power", "hold.time", "min.duration", "max.duration", "thinning", "filter")], 1, paste, collapse = "-")

        exp_grd <- exp_grd[!apply(exp_grd[, c("threshold", "power", "hold.time", "min.duration", "max.duration", "thinning", "filter")], 1, paste, collapse = "-") %in% previous.output$temp.label, ]

        # remove composed variable
        previous.output$temp.label <- NULL
            }


      if (nrow(exp_grd) == 0){
        cat("all combinations were already evaluated on previous call to this function (based on 'pevious.output')")

        return(previous.output)
        } else {

      # warn about number of combinations
      cat(paste(nrow(exp_grd), "combinations will be evaluated:"))
       cat("\n")


       eng_det_l <- warbleR:::pblapply_wrblr_int(X = 1:nrow(exp_grd), pbar = pb, cl = 1, FUN = function(x){

          eng_det <- energy_detector(files = files, envelopes = NULL, threshold = exp_grd$threshold[x], ssmooth = exp_grd$ssmooth[x], min.duration = exp_grd$min.duration[x], max.duration = exp_grd$max.duration[x], thinning = exp_grd$thinning[x], parallel = parallel, pb = FALSE, power = exp_grd$power[x], hold.time = exp_grd$hold.time[x], bp = bp, path = path)

          # make factor a character vector
          eng_det$sound.files <- as.character(eng_det$sound.files)

          eng_det$..row.id <- 1:nrow(eng_det)

          eng_det <- eng_det[!is.na(eng_det$start), ]

         return(eng_det)
          })

      performance_l <- lapply(eng_det_l, function(Z) suppressWarnings(diagnose_detection(reference = reference, detection = Z, by.sound.file = by.sound.file, time.diagnostics = TRUE, pb = FALSE, parallel = parallel)))

      performance <- do.call(rbind, performance_l)

      # duplicate expand grid tuning parameters if by sound file
      if (by.sound.file)
        exp_grd <- exp_grd[rep(1:nrow(exp_grd), each = length(files)), ]

        performance <- data.frame(exp_grd, performance)

      if (!is.null(previous.output))
        performance <- rbind(previous.output, performance)

      return(performance)
    }
}
