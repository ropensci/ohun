#' @title Evaluate the performance of a signal detection procedure
#'
#' @description \code{diagnose_detection} evaluates the performance of a signal detection procedure comparing the output selection table to a reference selection table
#' @usage diagnose_detection(reference, detection, by.sound.file = FALSE,
#' time.diagnostics = FALSE, parallel = 1, pb = TRUE, path = NULL)
#' @param reference Data frame or 'selection.table' (following the warbleR package format) with the reference selections (start and end of the signals) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end". \strong{It must contain the reference selections that will be used for detection optimization}.
#' @param detection Data frame or 'selection.table' with the detections (start and end of the signals) that will be compared against the 'reference' selections. Must contained at least the following columns: "sound.files", "selec", "start" and "end". It can contain data for additional sound files not found in 'references'. In this case the routine assumes that no signals are found in those files, so detection from those files are all false positives.
#' @param by.sound.file Logical argument to control whether performance diagnostics are summarized across sound files (when \code{by.sound.file = FALSE}, when more than 1 sound file is included in 'reference') or shown separated by sound file. Default is \code{FALSE}.
#' @param time.diagnostics Logical argument to control if diagnostics related to the duration of the signals ("mean.duration.true.positives", "mean.duration.false.positives", "mean.duration.false.negatives" and "proportional.duration.true.positives") are returned (if \code{TRUE}). Default is \code{FALSE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param path Character string containing the directory path where the sound files are located. If supplied then duty cycle (fraction of a sound file in which sounds were detected)is also returned. This feature is more helpful for tuning an energy-based detection. Default is \code{NULL}.
#' @return A data frame including the following detection performance diagnostics:
#' \itemize{
#'  \item \code{true.positives}: number of signals in 'reference' that correspond to any detection. Matching is defined as some degree of overlap in time. In a perfect detection routine it should be equal to the number of rows in 'reference'.
#'  \item \code{false.positives}: number of detections that don't match any of the signals in 'reference'. In a perfect detection routine it should be 0.
#'  \item \code{false.negatives}: number of signals in 'reference' that were not detected (not found in 'detection'. In a perfect detection routine it should be 0.
#'  \item \code{split.positives}: number of signals in 'reference' that were overlapped by more than 1 detection (i.e. detections that were split). In a perfect detection routine it should be 0.
#'  \item \code{merged.positives}: number of signals in 'detection' that were overlapped by more than 1 detection (i.e. signals that were merged). In a perfect detection routine it should be 0.
#'  \item \code{mean.duration.true.positives}: mean duration of true positives (in s). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{mean.duration.false.positives}: mean duration of false positives (in s). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{mean.duration.false.negatives}: mean duration of false negatives (in s). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{overlap.to.true.positives}: ratio of the time overlap of true positives in 'detection' with its corresponding reference signal to the duration of the reference signal.
#'  \item \code{proportional.duration.true.positives}: ratio of duration of true positives to the duration of signals in 'reference'. In a perfect detection routine it should be 1. Based only on true positives that were not split or merged.
#'  \item \code{duty.cycle}: proportion of a sound file in which sounds were detected. Only included when \code{time.diagnostics = TRUE} and \code{path} is supplied. Useful when conducting energy-based detection as a perfect detection can be obtained with a very low amplitude threshold, which will detect everything, but will produce a duty cycle close to 1.
#'  \item \code{sensitivity}: Proportion of signals in 'reference' that were detected. In a perfect detection routine it should be 1.
#'  \item \code{specificity}: Proportion of detections that correspond to signals in 'reference' that were detected. In a perfect detection routine it should be 1.
#'  }
#' @export
#' @name diagnose_detection
#' @details The function evaluates the performance of a signal detection procedure by comparing its output selection table to a reference selection table in which all signals of interest have been selected.
#' @examples {
#' # load data
#' data("lbh_reference")
#'
#' # perfect detection
#' diagnose_detection(reference = lbh_reference, detection = lbh_reference)
#'
#' # missing one in detection
#' diagnose_detection(reference = lbh_reference, detection = lbh_reference[-1, ])
#'
#' # an extra one in detection
#' diagnose_detection(reference = lbh_reference[-1, ], detection = lbh_reference)
#'
#' # with time diagnostics
#' diagnose_detection(reference = lbh_reference[-1, ],
#' detection = lbh_reference, time.diagnostics = TRUE)
#'
#' # and extra sound file in reference
#' diagnose_detection(reference = lbh_reference,
#' detection =
#' lbh_reference[lbh_reference$sound.files != "lbh1", ])
#'
#' # and extra sound file in detection
#' diagnose_detection(reference =
#' lbh_reference[lbh_reference$sound.files != "lbh1", ],
#' detection = lbh_reference)
#'
#' # and extra sound file in detection by sound file
#' dd <- diagnose_detection(reference =
#' lbh_reference[lbh_reference$sound.files != "lbh1", ],
#' detection = lbh_reference, time.diagnostics = TRUE, by.sound.file = TRUE)
#'
#' # get summary
#' summarize_diagnostic(dd)
#' }
#' @seealso \code{\link{optimize_energy_detector}}, \code{\link{optimize_template_detector}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references {
#' Araya-Salas, M. (2021), ohun: automatic detection of acoustic signals. R package version 0.1.0.
#' }
# last modification on sept-2021 (MAS)
diagnose_detection <- function(reference, detection, by.sound.file = FALSE, time.diagnostics = FALSE, parallel = 1, pb = TRUE, path = NULL)
{

  # make it a data frame if selection table
  if (warbleR::is_selection_table(detection))
    detection <- as.data.frame(detection)

  # make it a data frame if selection table
  if (warbleR::is_selection_table(reference))
    reference <- as.data.frame(reference)

    # remove rows with no info
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
          reference$end[as.numeric(labeled_detection$reference.row[x])] - reference$start[as.numeric(labeled_detection$reference.row[x])]
    })

        # look at detections matching 1 training selection at the time
        performance_list <- lapply(unique(labeled_detection$sound.files), function(z){

          # get subset for that sound file
          sub_detec <- labeled_detection[labeled_detection$sound.files == z, ]
          sub_ref <- reference[reference$sound.files == z, ]

          # get row index in reference for detected signals
          detected_reference_rows <- unique(na.omit(unlist(lapply(sub_detec$reference.row, function(x) unlist(strsplit(as.character(x), "-"))))))

        performance <- data.frame(
              sound.files = z,
              true.positives = length(detected_reference_rows),
              false.positives = sum(grepl("false", sub_detec$detection.class)),
              false.negatives = sum(!sub_ref$..row.id %in% detected_reference_rows),
              split.positives = length(unique(
                unlist(lapply(sub_detec$reference.row[grepl("split)", sub_detec$detection.class)], function(x) unlist(strsplit(x, "-")))))),
              merged.positives = sum(grepl("merge", sub_detec$detection.class)),
              mean.duration.true.positives = mean((sub_detec$end - sub_detec$start)[grep("true", sub_detec$detection.class)]),
              mean.duration.false.positives = mean((sub_detec$end - sub_detec$start)[grep("false", sub_detec$detection.class)]),
              mean.duration.false.negatives = mean((sub_ref$end - sub_ref$start)[!sub_ref$..row.id %in% detected_reference_rows]),
              overlap.to.true.positives = if(any(!is.na(sub_detec$overlap))) mean(sub_detec$overlap, na.rm = TRUE) else NA,
              proportional.duration.true.positives = mean(sub_detec$reference.duration, na.rm = TRUE) / mean((sub_ref$end - sub_ref$start)[sub_ref$..row.id %in% detected_reference_rows], na.rm = TRUE),
              stringsAsFactors = FALSE
            )

        # add duty cycle
        if (!is.null(path) & time.diagnostics){

          # get file durations
          performance$duty.cycle <- sum((sub_detec$end - sub_detec$start), na.rm = TRUE) / warbleR::duration_sound_files(files = z, path = path)$duration
        }


        # add sensitivity and specificity
        performance$sensitivity <- length(detected_reference_rows) / nrow(sub_ref)
        performance$specificity <-  if (nrow(sub_detec) > 0 & length(detected_reference_rows) > 0) length(detected_reference_rows) / (nrow(sub_ref) + sum(grep("false", sub_detec$detection.class))) else 0

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
      if (any(!reference$sound.files %in% unique(labeled_detection$sound.files))){

       no_detec <- data.frame(
          sound.files = setdiff(reference$sound.files, unique(labeled_detection$sound.files)),
          true.positives = 0,
          false.positives = 0,
          false.negatives = sapply(setdiff(reference$sound.files, unique(labeled_detection$sound.files)), function(x) sum(reference$sound.files == x)),
          split.positives = NA,
          merged.positives = NA,
          mean.duration.true.positives = NA,
          mean.duration.false.positives = NA,
          mean.duration.false.negatives = sapply(setdiff(reference$sound.files, unique(labeled_detection$sound.files)), function(x) mean((reference$end - reference$start)[reference$sound.files == x])),
          overlap.to.true.positives = NA,
          proportional.duration.true.positives = NA,
          sensitivity = 0,
          specificity =  0,
          stringsAsFactors = FALSE
        )

       # add duty cycle
       if (!is.null(path) & time.diagnostics)
         no_detec$duty.cycle <- 0

       performance_list[[length(performance_list) + 1]] <- no_detec

}
      # put in a single data frame
      performance_df <- do.call(rbind, performance_list)

} else  {
  performance_df <- data.frame(
  sound.files = unique(reference$sound.files),
  true.positives = 0,
  false.positives = 0,
  false.negatives = sapply(unique(reference$sound.files), function(x) sum(reference$sound.files == x)),
  split.positives = NA,
  merged.positives = NA,
  mean.duration.true.positives = NA,
  mean.duration.false.positives = NA,
  mean.duration.false.negatives = sapply(unique(reference$sound.files), function(x) mean(reference$end - reference$start)),
  overlap.to.true.positives = NA,
  proportional.duration.true.positives = NA,
  sensitivity = 0,
  specificity =  0,
  stringsAsFactors = FALSE
)
  # add duty cycle
  if (!is.null(path) & time.diagnostics)
    performance_df$duty.cycle <- 0

}

  # sort columns
  performance_df <- performance_df[ , na.omit(match(c("sound.files", "true.positives", "false.positives", "false.negatives", "split.positives", "merged.positives", "mean.duration.true.positives", "mean.duration.false.positives", "mean.duration.false.negatives", "overlap.to.true.positives", "proportional.duration.true.positives", "duty.cycle", "sensitivity", "specificity"), names(performance_df)))]

# summarize across sound files
if (!by.sound.file)
  performance_df <-
    summarize_diagnostic(diagnostic = performance_df, time.diagnostics = time.diagnostics)

# remove time diagnostics
if (!time.diagnostics)
  performance_df <- performance_df[ , grep(".duration.|duty", names(performance_df), invert = TRUE)]

# fix row names
rownames(performance_df) <- 1:nrow(performance_df)
    return(performance_df)
}
