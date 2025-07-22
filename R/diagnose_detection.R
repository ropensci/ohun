#' @title Evaluate the performance of a sound event detection procedure
#'
#' @description \code{diagnose_detection} evaluates the performance of a sound event detection procedure comparing the output selection table to a reference selection table
#' @param reference Data frame or 'selection.table' (following the warbleR package format) with the reference selections (start and end of the sound events) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end". \strong{It must contain the reference selections that will be used for detection optimization}.
#' @param detection Data frame or 'selection.table' with the detections (start and end of the sound events) that will be compared against the 'reference' selections. Must contained at least the following columns: "sound.files", "selec", "start" and "end". It can contain data for additional sound files not found in 'references'. In this case the routine assumes that no sound events are found in those files, so detection from those files are all false positives.
#' @param by.sound.file Logical argument to control whether performance diagnostics are summarized across sound files (when \code{by.sound.file = FALSE}, when more than 1 sound file is included in 'reference') or shown separated by sound file. Default is \code{FALSE}.
#' @param time.diagnostics Logical argument to control if diagnostics related to the duration of the sound events ("mean.duration.true.positives", "mean.duration.false.positives", "mean.duration.false.negatives" and "proportional.duration.true.positives") are returned (if \code{TRUE}). Default is \code{FALSE}.
#' @param cores Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param path Character string containing the directory path where the sound files are located. If supplied then duty cycle (fraction of a sound file in which sounds were detected)is also returned. This feature is more helpful for tuning an energy-based detection. Default is \code{NULL}.
#' @param by Character vector with the name of a column in 'reference' for splitting diagnostics. Diagnostics will be returned separated for each level in 'by'. Default is \code{NULL}.
#' @param macro.average Logical argument to control if diagnostics are first calculated for each sound file and then averaged across sound files, which can minimize the effect of unbalanced sample sizes between sound files. If \code{FALSE} (default) diagnostics are based on aggregated statistics irrespective of sound files. The following indices can be estimated by macro-averaging: overlap, mean.duration.true.positives, mean.duration.false.positives, mean.duration.false.positives, mean.duration.false.negatives, proportional.duration.true.positives, recall and precision (f.score is always derived from recall and precision). Note that when applying macro-averaging, recall and precision are not derived from the true positive, false positive and false negative values returned by the function.
#' @param min.overlap Numeric. Controls the minimum amount of overlap required for a detection and a reference sound for it to be counted as true positive. Default is 0.5. Overlap is measured as intersection over union. Only used if \code{solve.ambiguous = TRUE}.
#' @param solve.ambiguous Logical argument to control whether ambiguous detections (i.e. split and merged positives) are solved using maximum bipartite graph matching. Default is \code{TRUE}. If \code{FALSE} ambiguous detections are not solved.
#' @return A data frame including the following detection performance diagnostics:
#' \itemize{
#'  \item \code{detections}: total number of detections
#'  \item \code{true.positives}: number of sound events in 'reference' that correspond to any detection. Matching is defined as some degree of overlap in time. In a perfect detection routine it should be equal to the number of rows in 'reference'.
#'  \item \code{false.positives}: number of detections that don't match (i.e. don't overlap with) any of the sound events in 'reference'. In a perfect detection routine it should be 0.
#'  \item \code{false.negatives}: number of sound events in 'reference' that were not detected (not found in 'detection'. In a perfect detection routine it should be 0.
#'  \item \code{splits}: number of detections overlapping reference sounds that also overlap with other detections. In a perfect detection routine it should be 0.
#'  \item \code{merges}: number of detections that overlap with two or more reference sounds. In a perfect detection routine it should be 0.
#'  \item \code{mean.duration.true.positives}: mean duration of true positives (in ms). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{mean.duration.false.positives}: mean duration of false positives (in ms). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{mean.duration.false.negatives}: mean duration of false negatives (in ms). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{overlap}: mean intersection over union overlap of true positives.
#'  \item \code{proportional.duration.true.positives}: ratio of duration of true positives to the duration of sound events in 'reference'. In a perfect detection routine it should be 1. Based only on true positives that were not split or merged.
#'  \item \code{duty.cycle}: proportion of a sound file in which sounds were detected. Only included when \code{time.diagnostics = TRUE} and \code{path} is supplied. Useful when conducting energy-based detection as a perfect detection can be obtained with a very low amplitude threshold, which will detect everything, but will produce a duty cycle close to 1.
#'  \item \code{recall}: Proportion of sound events in 'reference' that were detected. In a perfect detection routine it should be 1.
#'  \item \code{precision}: Proportion of detections that correspond to sound events in 'reference'. In a perfect detection routine it should be 1.
#'  \item \code{f.score}: Combines recall and precision as the harmonic mean of these two. Provides a single value for evaluating performance. In a perfect detection routine it should be 1.
#'  }
#' @export
#' @name diagnose_detection
#' @details The function evaluates the performance of a sound event detection procedure by comparing its output selection table to a reference selection table in which all sound events of interest have been selected. The function takes any overlap between detected sound events and target sound events as true positives. Note that all sound files located in the supplied 'path' will be analyzed even if not all of them are listed in 'reference'. When several possible matching pairs of sound event and detections are found, the optimal set of matching pairs is found through maximum bipartite matching (using the R package igraph). Priority for assigning a detection to a reference is given by the amount of time overlap. 'splits' and 'merge.positives' are also counted (i.e. counted twice) as 'true.positives'. Therefore "true.positives + false.positives = detections".
#' @examples {
#'   # load data
#'   data("lbh_reference")
#'
#'   # perfect detection
#'   diagnose_detection(reference = lbh_reference, detection = lbh_reference)
#'
#'   # missing one in detection
#'   diagnose_detection(reference = lbh_reference, detection = lbh_reference[-1, ])
#'
#'   # an extra one in detection
#'   diagnose_detection(reference = lbh_reference[-1, ], detection = lbh_reference)
#'
#'   # with time diagnostics
#'   diagnose_detection(
#'     reference = lbh_reference[-1, ],
#'     detection = lbh_reference, time.diagnostics = TRUE
#'   )
#'
#'   # and extra sound file in reference
#'   diagnose_detection(
#'     reference = lbh_reference,
#'     detection =
#'       lbh_reference[lbh_reference$sound.files != "lbh1", ]
#'   )
#'
#'   # and extra sound file in detection
#'   diagnose_detection(
#'     reference =
#'       lbh_reference[lbh_reference$sound.files != "lbh1", ],
#'     detection = lbh_reference
#'   )
#'
#'   # and extra sound file in detection by sound file
#'   dd <- diagnose_detection(
#'     reference =
#'       lbh_reference[lbh_reference$sound.files != "lbh1", ],
#'     detection = lbh_reference, time.diagnostics = TRUE, by.sound.file = TRUE
#'   )
#'
#'   # get summary
#'   summarize_diagnostic(dd)
#' }
#' @seealso \code{\link{optimize_energy_detector}}, \code{\link{optimize_template_detector}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references 
#'  Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C., Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. (2023). ohun: An R package for diagnosing and optimizing automatic sound event detection. Methods in Ecology and Evolution, 14, 2259–2271. https://doi.org/10.1111/2041-210X.14170

diagnose_detection <-
  function(reference,
           detection,
           by.sound.file = FALSE,
           time.diagnostics = FALSE,
           cores = 1,
           pb = TRUE,
           path = NULL,
           by = NULL,
           macro.average = FALSE,
           min.overlap = 0.5,
           solve.ambiguous = TRUE
           ) {
    # check arguments
    if (options("ohun_check_args")$ohun_check_args) {
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
    }

    # do not check arguments on internal ohun function here (label_detection())
    options(ohun_check_args = FALSE)
    on.exit(options(ohun_check_args = TRUE))

    # do it by
    # run the function for each subset split by "by"
    if (!is.null(by)) {
      split_det <- split(x = detection, f = detection[, by])

      split_diagnostic <-
        warbleR:::.pblapply(
          X = seq_len(length(split_det)),
          cl = 1,
          pbar = pb,
          message = "diagnose_detection",
          total = 1,
          FUN = function(x) {
            by_diag <-
              diagnose_detection(
                reference = reference,
                detection = split_det[[x]],
                pb = FALSE,
                cores = cores,
                time.diagnostics = time.diagnostics,
                path = path,
                by.sound.file = by.sound.file,
                macro.average = macro.average
              )

            # add by label
            by_diag$by <- names(split_det)[x]

            # order columns
            by_diag <- by_diag[, c(ncol(by_diag), 1:(ncol(by_diag) - 1))]

            # rename by column
            names(by_diag)[1] <- by

            return(by_diag)
          }
        )

      performance_df <- do.call(rbind, split_diagnostic)
    } else {
      # make it a data frame if selection table
      if (warbleR::is_selection_table(detection)) {
        detection <- as.data.frame(detection)
      }

      # make it a data frame if selection table
      if (warbleR::is_selection_table(reference)) {
        reference <- as.data.frame(reference)
      }

      # remove rows with no info
      detection <- detection[!is.na(detection$start), ]

      # message if more sound files in detection than in reference
      extra_detec_sf <-
        setdiff(detection$sound.files, reference$sound.files)

      if (length(extra_detec_sf)) {
        on.exit(warning(
          "There is at least one additional sound file in 'detection' not found in 'reference'"
        ))
      }

      if (nrow(detection) > 0) {
        # double checking happens inside label_detection()
        labeled_detection <-
          label_detection(
            reference = reference,
            detection = detection,
            cores = cores,
            pb = pb,
            min.overlap = min.overlap,
            solve.ambiguous = solve.ambiguous
          )


        # add overlaps as attributes
        overlaps <- attributes(labeled_detection)$overlaps

        # look at detections matching 1 reference selection at the time
        performance_list <-
          lapply(unique(labeled_detection$sound.files), function(z) {
            # get subset of detections for that sound file
            sub_detec <-
              labeled_detection[labeled_detection$sound.files == z, ]
            sub_detec$id <- paste(sub_detec$sound.files, sub_detec$selec, sep = "-")
            # get subset of detections for that sound file
            sub_ref <- reference[reference$sound.files == z, ]
            sub_ref$id <- paste(sub_ref$sound.files, sub_ref$selec, sep = "-")

            # get subset of overlaps for that sound file
            sub_overlaps <- overlaps[overlaps$sound.files == z, ]

            # put all performance indices in a data frame
            performance <- data.frame(
              sound.files = z,
              detections = nrow(sub_detec),
              true.positives = sum(
                grepl("^true.positive", x = sub_detec$detection.class)
              ),
              false.positives = sum(grepl(
                "^false.positive", sub_detec$detection.class
              )),
              false.negatives = sum(!sub_ref$id %in% sub_overlaps$reference.id),
              splits = sum(grepl(
                "split", sub_detec$detection.class
              )),
              merges = sum(grepl(
                "merged", sub_detec$detection.class
              )),
              mean.duration.true.positives = round(mean((sub_detec$end - sub_detec$start)[grep("true", sub_detec$detection.class)],
                na.rm = TRUE
              ) * 1000, 0),
              mean.duration.false.positives = round(mean((sub_detec$end - sub_detec$start)[grep("false", sub_detec$detection.class)],
                na.rm = TRUE
              ) * 1000, 0),
              mean.duration.false.negatives = round(mean((
                sub_ref$end - sub_ref$start
              )[!sub_ref$id %in% sub_overlaps$reference.id]) * 1000, 0),
              overlap = if (nrow(sub_overlaps) > 1) {
                mean(sub_overlaps$IoU, na.rm = TRUE)
              } else {
                NA
              },
              proportional.duration.true.positives = mean((sub_detec$end - sub_detec$start)[grep("true", sub_detec$detection.class)], na.rm = TRUE) / mean((sub_ref$end - sub_ref$start)[sub_ref$id %in% sub_overlaps$reference.id], na.rm = TRUE),
              stringsAsFactors = FALSE
            )

            # add duty cycle
            if (!is.null(path) & time.diagnostics) {
              # get file durations
              performance$duty.cycle <-
                sum((sub_detec$end - sub_detec$start), na.rm = TRUE) / warbleR::duration_sound_files(files = z, path = path)$duration
            }

            # add recall, precision and f score
            performance$recall <-
              performance$true.positives / nrow(sub_ref)
            performance$precision <-
              if (nrow(sub_detec) > 0 &
                performance$true.positives > 0) {
                (performance$true.positives / performance$detections)
              } else {
                0
              }

            performance$f.score <-
              2 * ((performance$precision * performance$recall) / (performance$precision + performance$recall)
              )

            # replace NaNs with NA
            for (i in seq_len(ncol(performance))) {
              if (is.nan(performance[, i])) {
                performance[, i] <- NA
              }
            }

            # fix values when no false positives or true positives
            performance$false.positives[performance$false.positives < 0] <-
              0
            performance$mean.duration.false.positives[is.na(performance$mean.duration.false.positives) |
              performance$false.positives == 0] <- NA
            performance$mean.duration.true.positives[is.na(performance$mean.duration.true.positives) |
              performance$true.positives == 0] <- NA

            return(performance)
          })

        # add diagnostics of files in reference but not in detection
        if (any(!reference$sound.files %in% unique(labeled_detection$sound.files))) {
          no_detec <- data.frame(
            sound.files = setdiff(
              reference$sound.files,
              unique(labeled_detection$sound.files)
            ),
            detections = 0,
            true.positives = 0,
            false.positives = 0,
            false.negatives = vapply(setdiff(
              reference$sound.files,
              unique(labeled_detection$sound.files)
            ), function(x) {
              sum(reference$sound.files == x)
            }, FUN.VALUE = numeric(1)),
            splits = NA,
            merges = NA,
            mean.duration.true.positives = NA,
            mean.duration.false.positives = NA,
            mean.duration.false.negatives = vapply(setdiff(
              reference$sound.files,
              unique(labeled_detection$sound.files)
            ), function(x) {
              mean((reference$end - reference$start)[reference$sound.files == x])
            }, FUN.VALUE = numeric(1)),
            overlap = NA,
            proportional.duration.true.positives = NA,
            recall = 0,
            precision = 0,
            f.score = 0,
            stringsAsFactors = FALSE
          )

          # add duty cycle
          if (!is.null(path) & time.diagnostics) {
            no_detec$duty.cycle <- 0
          }

          performance_list[[length(performance_list) + 1]] <- no_detec
        }
        # put in a single data frame
        performance_df <- do.call(rbind, performance_list)
      } else {
        performance_df <- data.frame(
          sound.files = unique(reference$sound.files),
          detections = 0,
          true.positives = 0,
          false.positives = 0,
          false.negatives = vapply(unique(reference$sound.files), function(x) {
            sum(reference$sound.files == x)
          }, FUN.VALUE = numeric(1)),
          splits = NA,
          merges = NA,
          mean.duration.true.positives = NA,
          mean.duration.false.positives = NA,
          mean.duration.false.negatives = vapply(unique(reference$sound.files), function(x) {
            mean(reference$end - reference$start)
          }, FUN.VALUE = numeric(1)) * 1000,
          overlap = NA,
          proportional.duration.true.positives = NA,
          recall = 0,
          precision = 0,
          f.score = 0,
          stringsAsFactors = FALSE
        )
        # add duty cycle
        if (!is.null(path) & time.diagnostics) {
          performance_df$duty.cycle <- 0
        }
      }

      # sort columns
      performance_df <-
        performance_df[, na.omit(match(
          c(
            "sound.files",
            "detections",
            "true.positives",
            "false.positives",
            "false.negatives",
            "splits",
            "merges",
            "mean.duration.true.positives",
            "mean.duration.false.positives",
            "mean.duration.false.negatives",
            "overlap",
            "proportional.duration.true.positives",
            "duty.cycle",
            "recall",
            "precision",
            "f.score"
          ),
          names(performance_df)
        ))]

      # summarize across sound files
      if (!by.sound.file) {
        if (nrow(performance_df) > 1) {
          performance_df <-
            summarize_diagnostic(diagnostic = performance_df, time.diagnostics = time.diagnostics, macro.average = macro.average)
        } else {
          performance_df$sound.files <- NULL
        }
      }

      # remove time diagnostics
      if (!time.diagnostics) {
        performance_df <-
          performance_df[, grep(".duration.|duty", names(performance_df), invert = TRUE)]
      }

      # fix row names
      rownames(performance_df) <- seq_len(nrow(performance_df))
    }
    return(performance_df)
  }
