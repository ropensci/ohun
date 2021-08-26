#' @title Evaluate the performance of a signal detection procedure
#'
#' @description \code{diagnose_detection} evaluates the performance of a signal detection procedure comparing the output selection table to a reference selection table
#' @usage diagnose_detection(reference, detection, by.sound.file = FALSE,
#' time.diagnostics = FALSE, parallel = 1, pb = TRUE)
#' @param reference Data frame or 'selection.table' (following the warbleR package format) with the reference selections (start and end of the signals) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end". \strong{It should contain the reference selections that will be used for detection optimization}.
#' @param detection Data frame or 'selection.table' with the detections (start and end of the signals) that will be compared against the 'reference' selections. Must contained at least the following columns: "sound.files", "selec", "start" and "end". It can contain data for additional sound files not found in 'references'. In this case the routine assumes that no signals are found in those files, so detection from those files are all false positives.
#' @param by.sound.file Logical argument to control whether performance diagnostics are summarized across sound files (when \code{by.sound.file = FALSE}, when more than 1 sound file is included in 'reference') or shown separated by sound file. Default is \code{FALSE}.
#' @param time.diagnostics Logical argument to control if diagnostics related to the duration of the signals ("mean.duration.true.positives", "mean.duration.false.positives", "mean.duration.false.negatives" and "proportional.duration.true.positives") are returned (if \code{TRUE}). Default is \code{FALSE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return A data frame including the following detection performance diagnostics:
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
#' @export
#' @name diagnose_detection
#' @details The function evaluates the performance of a signal detection procedure by comparing its output selection table to a reference selection table in which all signals of interest have been selected.
#' @examples {
#' # perfect detection
#' diagnose_detection(reference = lbh_selec_reference, detection = lbh_selec_reference)
#'
#' # missing one in detection
#' diagnose_detection(reference = lbh_selec_reference, detection = lbh_selec_reference[-1, ])
#'
#' # an extra one in detection
#' diagnose_detection(reference = lbh_selec_reference[-1, ], detection = lbh_selec_reference)
#'
#' # with time diagnostics
#' diagnose_detection(reference = lbh_selec_reference[-1, ],
#' detection = lbh_selec_reference, time.diagnostics = TRUE)
#'
#' # and extra sound file in reference
#' diagnose_detection(reference = lbh_selec_reference,
#' detection =
#' lbh_selec_reference[lbh_selec_reference$sound.files != "Phae.long1.wav", ])
#'
#' # and extra sound file in detection
#' diagnose_detection(reference =
#' lbh_selec_reference[lbh_selec_reference$sound.files != "Phae.long1.wav", ],
#' detection = lbh_selec_reference)
#'
#' # and extra sound file in detection by sound file
#' dd <- diagnose_detection(reference =
#' lbh_selec_reference[lbh_selec_reference$sound.files != "Phae.long1.wav", ],
#' detection = lbh_selec_reference, time.diagnostics = TRUE, by.sound.file = TRUE)
#'
#' # get summary
#' summarize_diagnostic(dd)
#'
#'
#' }
#' @seealso \code{\link{optimize_auto_detec}}, \code{\link{optimize_find_peaks}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references {
#' Araya-Salas, M. (2021), ohun: automatic detection of acoustic signals. R package version 0.1.0.
#' }
# last modification on jul-16-2021 (MAS)
diagnose_detection <- function(reference, detection, by.sound.file = FALSE, time.diagnostics = FALSE, parallel = 1, pb = TRUE)
{


  # remove row with no info
  detection <- detection[!is.na(detection$start), ]

  if (nrow(detection) > 0)
  {
    # double checking happens inside label_detection()
    labeled_detection <- label_detection(reference = reference, detection = detection, parallel = parallel, pb = pb)

    # # add row labels to reference for getting false negatives
    reference$..row.id <- 1:nrow(reference)

    # duration of corresponding selection in reference
    labeled_detection$reference.duration <- sapply(1:nrow(labeled_detection), function(x){
      if (is.na(labeled_detection$reference.row[x])) NA  else
        if (grepl("-", labeled_detection$reference.row[x]))  NA else
          labeled_detection$end[x] - labeled_detection$start[x]
    })

        # look at detections matching 1 training selection at the time
        performance_list <- lapply(unique(labeled_detection$sound.files), function(z){

          # get subset for that sound file
          sub_detec <- labeled_detection[labeled_detection$sound.files == z, ]
          sub_ref <- reference[reference$sound.files == z, ]

          # get row index in reference for detected signals
          detected_reference_rows <- unlist(sapply(sub_detec$reference.row, function(x) unlist(strsplit(x, "-")), USE.NAMES = FALSE))

        performance <- data.frame(
              sound.files = z,
              true.positives = sum(grepl("true", sub_detec$detection.class)),
              false.positives = sum(grepl("false", sub_detec$detection.class)),
              false.negatives = sum(!sub_ref$..row.id %in% detected_reference_rows),
              split.positives = sum(grepl("split", sub_detec$detection.class)) / 2,
              merged.positives = sum(grepl("merge", sub_detec$detection.class)),
              mean.duration.true.positives = mean((sub_detec$end - sub_detec$start)[grep("true", sub_detec$detection.class)]),
              mean.duration.false.positives = mean((sub_detec$end - sub_detec$start)[grep("false", sub_detec$detection.class)]),
              mean.duration.false.negatives = mean((sub_ref$end - sub_ref$start)[!sub_ref$..row.id %in% detected_reference_rows]),
              proportional.duration.true.positives = mean(sub_detec$reference.duration, na.rm = TRUE) / mean((sub_ref$end - sub_ref$start)[sub_ref$..row.id %in% detected_reference_rows], na.rm = TRUE),
              sensitivity = sum(!is.na(unique(detected_reference_rows))) / nrow(sub_ref),
              specificity =  if (nrow(sub_detec) > 0) sum(!is.na(unique(detected_reference_rows))) / (nrow(sub_ref) + sum(grep("false", sub_detec$detection.class))) else 0,
              stringsAsFactors = FALSE
            )

          # replace NaNs with NA
          for(i in 1:ncol(performance))
            if (is.nan(performance[, i])) performance[, i] <- NA

          # fix values when no false positives or true positives
          performance$false.positives[performance$false.positives < 0] <- 0
          performance$mean.duration.false.positives[is.na(performance$mean.duration.false.positives) | performance$false.positives == 0] <- NA
          performance$mean.duration.true.positives[is.na(performance$mean.duration.true.positives) | performance$true.positives == 0] <- NA

          # make sensitvities higher than 1 (because of split positives) 1
          performance$sensitivity[performance$sensitivity > 1] <- 1

          return(performance)
          })


      # add diagnostics of files in reference but not in detection
      if (any(!reference$sound.files %in% unique(labeled_detection$sound.files)))

        performance_list[[length(performance_list) + 1]] <- data.frame(
          sound.files = setdiff(reference$sound.files, unique(labeled_detection$sound.files)),
          true.positives = 0,
          false.positives = 0,
          false.negatives = sapply(setdiff(reference$sound.files, unique(labeled_detection$sound.files)), function(x) sum(reference$sound.files == x)),
          split.positives = NA,
          merged.positives = NA,
          mean.duration.true.positives = NA,
          mean.duration.false.positives = NA,
          mean.duration.false.negatives = sapply(setdiff(reference$sound.files, unique(labeled_detection$sound.files)), function(x) mean((reference$end - reference$start)[reference$sound.files == x])),
          proportional.duration.true.positives = NA,
          sensitivity = 0,
          specificity =  0,
          stringsAsFactors = FALSE
        )


      # put in a single data frame
      performance_df <- do.call(rbind, performance_list)

} else  performance_df <- data.frame(
  sound.files = unique(reference$sound.files),
  true.positives = 0,
  false.positives = 0,
  false.negatives = sapply(unique(reference$sound.files), function(x) sum(reference$sound.files == x)),
  split.positives = NA,
  merged.positives = NA,
  mean.duration.true.positives = NA,
  mean.duration.false.positives = NA,
  mean.duration.false.negatives = sapply(unique(reference$sound.files), function(x) mean(reference$end - reference$start)),
  proportional.duration.true.positives = NA,
  sensitivity = 0,
  specificity =  0,
  stringsAsFactors = FALSE
  )

# summarize across sound files
if (!by.sound.file)
  performance_df <- summarize_diagnostic(diagnostic = performance_df, time.diagnostics = time.diagnostics)

# remove time diagnostics
if (!time.diagnostics)
  performance_df <- performance_df[ , grep(".duration.", names(performance_df), invert = TRUE)]

# fix row names
rownames(performance_df) <- 1:nrow(performance_df)
    return(performance_df)
}
