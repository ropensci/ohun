#' @title Optimize energy-based sound event detection
#'
#' @description Optimize energy-based sound event detection under different correlation threshold values
#' @usage optimize_energy_detector(reference, files = NULL, threshold = 5,
#' peak.amplitude = 0, hop.size = 11.6, wl = NULL, smooth = 5, hold.time = 0,
#' min.duration = NULL, max.duration = NULL, thinning = 1, cores = 1, pb = TRUE,
#'  by.sound.file = FALSE, bp = NULL, path = ".", previous.output = NULL, envelopes = NULL,
#'  macro.average = FALSE, min.overlap = 0.5)
#' @param reference Selection table (using the warbleR package's format, see \code{\link[warbleR]{selection_table}}) or data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of sound event
#' (start and end). \strong{It must contain the reference selections that will be used for detection optimization}.
#' @param files Character vector indicating the sound files that will be analyzed. Optional. If  not supplied the function will work on the sound files in 'reference'. It can be used to include sound files with no target sound events. Supported file formats:'.wav', '.mp3', '.flac' and '.wac'. If not supplied the function will work on all sound files (in the supported format) in 'path'.
#' @param threshold A numeric vector specifying the amplitude threshold for detecting
#'   sound events (in \%). Default is 5. \strong{Several values can be supplied for optimization}.
#' @param peak.amplitude Numeric vector of length 1 with the minimum peak amplitude value. A detection below that value would be excluded. Peak amplitude is the maximum sound pressure level (in decibels) across the sound event (see \code{\link[warbleR]{sound_pressure_level}}). This can be useful when expecting higher peak amplitude in the target sound events compared to non-target sound events or when keeping only the best examples of the target sound events (i.e. high precision and low recall). Default is 0. \strong{Several values can be supplied for optimization}.
#' @param hop.size A numeric vector of length 1 specifying the time window duration (in ms). Default is 11.6 ms, which is equivalent to 512 wl for a 44.1 kHz sampling rate. Ignored if 'wl' is supplied.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram. Default is \code{NULL}. If supplied, 'hop.size' is ignored. Used internally for bandpass filtering (so only applied when 'bp' is supplied).
#' @param smooth A numeric vector of length 1 to smooth the amplitude envelope
#'   with a sum smooth function. It controls the time 'neighborhood' (in ms) in which amplitude samples are smoothed (i.e. averaged with neighboring samples). Default is 5. 0 means no smoothing is applied. Note that smoothing is applied before thinning (see 'thinning' argument). The function  \code{\link[warbleR]{envelope}} is used internally which is analogous to sum smoothing in \code{\link[seewave]{env}}. This argument is used internally by \code{\link{get_envelopes}}. \strong{Several values can be supplied for optimization}.
#' @param hold.time Numeric vector of length 1. Specifies the time range (in ms) at which selections will be merged (i.e. if 2 selections are separated by less than the specified 'hold.time' they will be merged in to a single selection). Default is \code{0} (no hold time applied). \strong{Several values can be supplied for optimization}.
#' @param min.duration Numeric vector giving the shortest duration (in
#'   ms) of the sound events to be detected. It removes sound events below that
#'   threshold. \strong{Several values can be supplied for optimization}.
#' @param max.duration Numeric vector giving the longest duration (in
#'   ms) of the sound events to be detected. It removes sound events above that
#'   threshold. \strong{Several values can be supplied for optimization}.
#' @param thinning Numeric vector in the range 0~1 indicating the proportional reduction of the number of
#' samples used to represent amplitude envelopes (i.e. the thinning of the envelopes). Usually amplitude envelopes have many more samples
#' than those needed to accurately represent amplitude variation in time, which affects the size of the
#' output (usually very large R objects / files). Default is  \code{1} (no thinning). Higher sampling rates may afford higher size reduction (e.g. lower thinning values). Reduction is conducted by interpolation using \code{\link[stats]{approx}}. Note that thinning may decrease time precision, and the higher the thinning the less precise the time detection. \strong{Several values can be supplied for optimization}.
#' @param cores Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @param by.sound.file Logical argument to control whether performance diagnostics are summarized across sound files (when \code{by.sound.file = FALSE} and more than 1 sound file is included in 'reference') or shown separated by sound file. Default is \code{FALSE}.
#' @param bp Numeric vector of length 2 giving the lower and upper limits of a
#'   frequency bandpass filter (in kHz). Default is \code{NULL}.  This argument is used internally by \code{\link{get_envelopes}}. Not used if 'envelopes' are supplied. Bandpass is done using the function \code{\link[seewave]{ffilter}}, which applies a short-term Fourier transformation to first create a spectrogram in which the target frequencies are filtered and then is back transformed into a wave object using a reverse Fourier transformation.
#' @param path Character string containing the directory path where the sound files are located.
#' The current working directory is used as default.
#' @param previous.output Data frame with the output of a previous run of this function. This will be used to include previous results in the new output and avoid recalculating detection performance for parameter combinations previously evaluated.
#' @param envelopes An object of class 'envelopes' (generated by \code{\link{get_envelopes}}) containing the amplitude envelopes of the sound files to be analyzed. If 'files' and 'envelopes' are not supplied then the function will work on all supported format sound files in the working directory.
#' @param macro.average Logical argument to control if diagnostics are first calculated for each sound file and then averaged across sound files, which can minimize the effect of unbalanced sample sizes between sound files. If \code{FALSE} (default) diagnostics are based on aggregated statistics irrespective of sound files. The following indices can be estimated by macro-averaging: overlap, mean.duration.true.positives, mean.duration.false.positives, mean.duration.false.positives, mean.duration.false.negatives, proportional.duration.true.positives, recall and precision (f.score is always derived from recall and precision). Note that when applying macro-averaging, recall and precision are not derived from the true positive, false positive and false negative values returned by the function.
#' @param min.overlap Numeric. Controls the minimum amount of overlap required for a detection and a reference sound for it to be counted as true positive. Default is 0.5. Overlap is measured as intersection over union.
#' @return A data frame in which each row shows the result of a detection job with a particular combination of tuning parameters (including in the data frame). It also includes the following diagnostic metrics:
#' \itemize{
#'  \item \code{true.positives}: number of sound events in 'reference' that correspond to any detection. Matching is defined as some degree of overlap in time. In a perfect detection routine it should be equal to the number of rows in 'reference'.
#'  \item \code{false.positives}: number of detections that don't match any of the sound events in 'reference'. In a perfect detection routine it should be 0.
#'  \item \code{false.negatives}: number of sound events in 'reference' that were not detected (not found in 'detection'. In a perfect detection routine it should be 0.
#'  \item \code{splits}: number of detections overlapping reference sounds that also overlap with other detections. In a perfect detection routine it should be 0.
#'  \item \code{merges}: number of detections that overlap with two or more reference sounds. In a perfect detection routine it should be 0.
#'  \item \code{mean.duration.true.positives}: mean duration of true positives (in ms). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{mean.duration.false.positives}: mean duration of false positives (in ms). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{mean.duration.false.negatives}: mean duration of false negatives (in ms). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{overlap}: mean intersection over union overlap of true positives.
#'  \item \code{proportional.duration.true.positives}: ratio of duration of true positives to th duration of sound events in 'reference'. In a perfect detection routine it should be 1. Based only on true positives that were not split or merged. Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{duty.cycle}: proportion of a sound file in which sounds were detected. Only included when \code{time.diagnostics = TRUE} and \code{path} is supplied.
#'  \item \code{recall}: Proportion of sound events in 'reference' that were detected. In a perfect detection routine it should be 1.
#'  \item \code{precision}: Proportion of detections that correspond to sound events in 'reference'. In a perfect detection routine it should be 1.
#'  }
#' @export
#' @name optimize_energy_detector
#' @details This function takes a selections data frame or 'selection_table' ('reference') estimates the detection performance of a energy detector under different detection parameter combinations. This is done by comparing the position in time of the detection to those of the reference selections in 'reference'. The function returns several diagnostic metrics to allow user to determine which parameter values provide a detection that more closely matches the selections in 'reference'. Those parameters can be later used for performing a more efficient detection using \code{\link{energy_detector}}.
#'
#' @examples \donttest{
#' # Save example files into temporary working directory
#' data("lbh1", "lbh2", "lbh_reference")
#' tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
#' tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
#'
#' # using smoothing and minimum duration
#' optimize_energy_detector(
#'   reference = lbh_reference, path = tempdir(),
#'   threshold = c(6, 10), smooth = 6.8, bp = c(2, 9), hop.size = 6.8,
#'   min.duration = 90
#' )
#'
#' # with thinning and smoothing
#' optimize_energy_detector(
#'   reference = lbh_reference, path = tempdir(),
#'   threshold = c(6, 10, 15), smooth = c(7, 10), thinning = c(0.1, 0.01),
#'   bp = c(2, 9), hop.size = 6.8, min.duration = 90
#' )
#'
#' # by sound file
#' (opt_ed <- optimize_energy_detector(
#'   reference = lbh_reference,
#'   path = tempdir(), threshold = c(6, 10, 15), smooth = 6.8, bp = c(2, 9),
#'   hop.size = 6.8, min.duration = 90, by.sound.file = TRUE
#' ))
#'
#' # summarize
#' summarize_diagnostic(opt_ed)
#'
#' # using hold time
#' (op_ed <- optimize_energy_detector(
#'   reference = lbh_reference,
#'   threshold = 10, hold.time = c(100, 150), bp = c(2, 9), hop.size = 6.8,
#'   path = tempdir()
#' ))
#'
#' # including previous output in new call
#' optimize_energy_detector(
#'   reference = lbh_reference, threshold = 10,
#'   hold.time = c(50, 200), previous.output = op_ed, smooth = 6.8,
#'   bp = c(2, 9), hop.size = 7, path = tempdir()
#' )
#'
#' # having and extra file in files (simulating a file that should have no detetions)
#' sub_reference <- lbh_reference[lbh_reference$sound.files != "lbh1.wav", ]
#'
#' optimize_energy_detector(
#'   reference = sub_reference, files = unique(lbh_reference$sound.files),
#'   threshold = 10, hold.time = c(1, 150), bp = c(2, 9), smooth = 6.8,
#'   hop.size = 7, path = tempdir()
#' )
#' }
#'
#' @references 
#' Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C., Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. 2022. ohun: an R package for diagnosing and optimizing automatic sound event detection. BioRxiv, 2022.12.13.520253. https://doi.org/10.1101/2022.12.13.520253
#' 
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}).

optimize_energy_detector <-
  function(reference,
           files = NULL,
           threshold = 5,
           peak.amplitude = 0,
           hop.size = 11.6,
           wl = NULL,
           smooth = 5,
           hold.time = 0,
           min.duration = NULL,
           max.duration = NULL,
           thinning = 1,
           cores = 1,
           pb = TRUE,
           by.sound.file = FALSE,
           bp = NULL,
           path = ".",
           previous.output = NULL,
           envelopes = NULL,
           macro.average = FALSE,
           min.overlap = 0.5) {
    # check arguments
    arguments <- as.list(base::match.call())

    # add objects to argument names
    for (i in names(arguments)[-1]) {
      arguments[[i]] <- get(i)
    }

    # check each arguments
    check_results <- check_arguments(fun = arguments[[1]], args = arguments)

    # report errors
    checkmate::reportAssertions(check_results)

    # check path if not provided set to working directory
    path <- if (is.null(path)) {
      getwd()
    } else {
      normalizePath(path)
    }

    # do not check arguments on internal ohun function here (energy_detector())
    options(ohun_check_args = FALSE)
    on.exit(options(ohun_check_args = TRUE))

    # if files not supplied then used those from reference
    if (is.null(files)) {
      files <- unique(reference$sound.files)
    }

    # if 'files' are found in reference
    if (!any(files %in% reference$sound.files)) {
      stop2("Not a single sound file in the working directory is found in 'reference'")
    }

    # get all possible combinations of parameters
    exp_grd <-
      expand.grid(
        threshold = threshold,
        peak.amplitude = peak.amplitude,
        smooth = smooth,
        hold.time = hold.time,
        min.duration = if (is.null(min.duration)) {
          -Inf
        } else {
          min.duration
        },
        max.duration = if (is.null(max.duration)) {
          Inf
        } else {
          max.duration
        },
        thinning = thinning
      )

    # if previous output included
    if (!is.null(previous.output)) {
      # create composed variable to find overlapping runs
      previous.output$temp.label <-
        apply(previous.output[, c(
          "threshold",
          "peak.amplitude",
          "smooth",
          "hold.time",
          "min.duration",
          "max.duration",
          "thinning"
        )], 1, paste, collapse = "-")

      exp_grd <-
        exp_grd[!apply(exp_grd[, c(
          "threshold",
          "peak.amplitude",
          "smooth",
          "hold.time",
          "min.duration",
          "max.duration",
          "thinning"
        )], 1, paste, collapse = "-") %in% previous.output$temp.label, ]

      # remove composed variable
      previous.output$temp.label <- NULL
    }


    if (nrow(exp_grd) == 0) {
      cat(
        "all combinations were already evaluated on previous call to this function (based on 'pevious.output')"
      )

      return(previous.output)
    } else {
      # warn about number of combinations
      cat(paste(nrow(exp_grd), "combinations will be evaluated:"))
      cat("\n")

      eng_det_l <-
        warbleR:::.pblapply(
          X = seq_len(nrow(exp_grd)),
          pbar = pb,
          cl = 1,
          message = "evaluating combinations",
          total = 1,
          FUN = function(x) {
            eng_det <-
              energy_detector(
                files = if (is.null(envelopes)) {
                  files
                } else {
                  NULL
                },
                envelopes = envelopes,
                threshold = exp_grd$threshold[x],
                peak.amplitude = exp_grd$peak.amplitude[x],
                smooth = exp_grd$smooth[x],
                min.duration = exp_grd$min.duration[x],
                max.duration = exp_grd$max.duration[x],
                thinning = exp_grd$thinning[x],
                cores = cores,
                pb = FALSE,
                hold.time = exp_grd$hold.time[x],
                bp = bp,
                path = path,
                hop.size = hop.size,
                wl = wl
              )

            # make factor a character vector
            eng_det$sound.files <- as.character(eng_det$sound.files)

            if (nrow(eng_det) > 0) {
              eng_det$..row.id <- seq_len(nrow(eng_det))
            }

            eng_det <- eng_det[!is.na(eng_det$start), ]

            return(eng_det)
          }
        )

      # do not check arguments on internal ohun function here (diagnose_detection() and energy_detector())
      options(ohun_check_args = FALSE)
      # on.exit(options(ohun_check_args = TRUE))

      performance_l <-
        lapply(eng_det_l, function(Z) {
          suppressWarnings(
            diagnose_detection(
              reference = reference,
              detection = Z,
              by.sound.file = by.sound.file,
              time.diagnostics = TRUE,
              pb = FALSE,
              cores = cores,
              path = path,
              macro.average = macro.average,
              min.overlap = min.overlap
            )
          )
        })

      performance <- do.call(rbind, performance_l)

      # duplicate expand grid tuning parameters if by sound file
      if (by.sound.file) {
        exp_grd <-
          exp_grd[rep(seq_len(nrow(exp_grd)), each = length(files)), ]
      }

      suppressWarnings(performance <-
        data.frame(exp_grd, performance))

      if (!is.null(previous.output)) {
        performance <- rbind(previous.output, performance)
      }

      # order colums
      performance <- warbleR::sort_colms(performance)

      # rename rows
      rownames(performance) <- seq_len(nrow(performance))

      return(performance)
    }
  }
