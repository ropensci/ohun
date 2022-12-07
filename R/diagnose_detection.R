#' @title Evaluate the performance of a sound event detection procedure
#'
#' @description \code{diagnose_detection} evaluates the performance of a sound event detection procedure comparing the output selection table to a reference selection table
#' @usage diagnose_detection(reference, detection, by.sound.file = FALSE,
#' time.diagnostics = FALSE, cores = 1, pb = TRUE, path = NULL, by = NULL)
#' @param reference Data frame or 'selection.table' (following the warbleR package format) with the reference selections (start and end of the sound events) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end". \strong{It must contain the reference selections that will be used for detection optimization}.
#' @param detection Data frame or 'selection.table' with the detections (start and end of the sound events) that will be compared against the 'reference' selections. Must contained at least the following columns: "sound.files", "selec", "start" and "end". It can contain data for additional sound files not found in 'references'. In this case the routine assumes that no sound events are found in those files, so detection from those files are all false positives.
#' @param by.sound.file Logical argument to control whether performance diagnostics are summarized across sound files (when \code{by.sound.file = FALSE}, when more than 1 sound file is included in 'reference') or shown separated by sound file. Default is \code{FALSE}.
#' @param time.diagnostics Logical argument to control if diagnostics related to the duration of the sound events ("mean.duration.true.positives", "mean.duration.false.positives", "mean.duration.false.negatives" and "proportional.duration.true.positives") are returned (if \code{TRUE}). Default is \code{FALSE}.
#' @param cores Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param path Character string containing the directory path where the sound files are located. If supplied then duty cycle (fraction of a sound file in which sounds were detected)is also returned. This feature is more helpful for tuning an energy-based detection. Default is \code{NULL}.
#' @param by Character vector with the name of a column in 'reference' for splitting diagnostics. Diagnostics will be returned separated for each level in 'by'. Default is \code{NULL}.
#' @return A data frame including the following detection performance diagnostics:
#' \itemize{
#'  \item \code{total.detections}: total number of detections
#'  \item \code{true.positives}: number of sound events in 'reference' that correspond to any detection. Matching is defined as some degree of overlap in time. In a perfect detection routine it should be equal to the number of rows in 'reference'.
#'  \item \code{false.positives}: number of detections that don't match (i.e. don't overlap with) any of the sound events in 'reference'. In a perfect detection routine it should be 0.
#'  \item \code{false.negatives}: number of sound events in 'reference' that were not detected (not found in 'detection'. In a perfect detection routine it should be 0.
#'  \item \code{split.positives}: number of sound events in 'reference' that were overlapped by more than 1 detection (i.e. detections that were split). In a perfect detection routine it should be 0.
#'  \item \code{merged.positives}: number of sound events in 'reference' that were overlapped by a detection that also overlaps with other sound events in 'reference' (i.e. sound events that were merged into a single detection). In a perfect detection routine it should be 0.
#'  \item \code{mean.duration.true.positives}: mean duration of true positives (in ms). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{mean.duration.false.positives}: mean duration of false positives (in ms). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{mean.duration.false.negatives}: mean duration of false negatives (in ms). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{overlap.to.true.positives}: ratio of the time overlap of true positives in 'detection' with its corresponding reference sound event to the duration of the reference sound event.
#'  \item \code{proportional.duration.true.positives}: ratio of duration of true positives to the duration of sound events in 'reference'. In a perfect detection routine it should be 1. Based only on true positives that were not split or merged.
#'  \item \code{duty.cycle}: proportion of a sound file in which sounds were detected. Only included when \code{time.diagnostics = TRUE} and \code{path} is supplied. Useful when conducting energy-based detection as a perfect detection can be obtained with a very low amplitude threshold, which will detect everything, but will produce a duty cycle close to 1.
#'  \item \code{recall}: Proportion of sound events in 'reference' that were detected. In a perfect detection routine it should be 1.
#'  \item \code{precision}: Proportion of detections that correspond to sound events in 'reference'. In a perfect detection routine it should be 1.
#'  \item \code{f1.score}: Combines recall and precision as the harmonic mean of these two. Provides a single value for evaluating performance. In a perfect detection routine it should be 1.
#'  }
#' @export
#' @name diagnose_detection
#' @details The function evaluates the performance of a sound event detection procedure by comparing its output selection table to a reference selection table in which all sound events of interest have been selected. The function takes any overlap between detected sound events and target sound events as true positives. Note that all sound files located in the supplied 'path' will be analyzed even if not all of them are listed in 'reference'. When several possible matching pairs of sound event and detections are found, the optimal set of matching pairs is found through bipartite graph matching (using the R package igraph). Priority for assigning a detection to a reference is given by the amount of time overlap.
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
#' Araya-Salas, M. (2021), ohun: diagnosing and optimizing automated sound event detection. R package version 0.1.0.
#' }
# last modification on sept-2021 (MAS)
diagnose_detection <-
  function(reference,
           detection,
           by.sound.file = FALSE,
           time.diagnostics = FALSE,
           cores = 1,
           pb = TRUE,
           path = NULL,
           by = NULL)
  {
    # do it by
    # run the function for each subset split by "by"
    if (!is.null(by)) {
      split_det <- split(x = detection, f = detection[, by])

      split_diagnostic <-
        warbleR:::pblapply_wrblr_int(
          X = 1:length(split_det),
          cl = 1,
          pbar = pb,
          FUN = function(x) {
            by_diag <-
              diagnose_detection(
                reference = reference,
                detection = split_det[[x]],
                pb = FALSE,
                cores = cores,
                time.diagnostics = time.diagnostics,
                path = path,
                by.sound.file = by.sound.file
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

    } else
    {
      # make it a data frame if selection table
      if (warbleR::is_selection_table(detection))
        detection <- as.data.frame(detection)

      # make it a data frame if selection table
      if (warbleR::is_selection_table(reference))
        reference <- as.data.frame(reference)

      # remove rows with no info
      detection <- detection[!is.na(detection$start),]

      # message if more sound files in detection than in reference
      extra_detec_sf <-
        setdiff(detection$sound.files, reference$sound.files)

      if (length(extra_detec_sf))
        on.exit(warning(
          "There is at least one additional sound file in 'detection' not found in 'reference'"
        ))

      if (nrow(detection) > 0)
      {
        # double checking happens inside label_detection()
        labeled_detection <-
          label_detection(
            reference = reference,
            detection = detection,
            cores = cores,
            pb = pb
          )

        # # add row labels to reference for getting false negatives
        reference$..row.id <- 1:nrow(reference)

        # duration of corresponding selection in reference
        labeled_detection$reference.duration <-
          sapply(1:nrow(labeled_detection), function(x) {
            if (is.na(labeled_detection$reference.row[x]) |
                !grepl("\\(", labeled_detection$reference.row[x]))
              NA else
            {
              # get matching reference
              list_ref_row <-
                strsplit(labeled_detection$reference.row[x], split = "\\(")[[1]]
              list_ref_row <- list_ref_row[length(list_ref_row)]
              list_ref_row <- as.numeric(gsub("\\)", "", list_ref_row))
              dur <-
                reference$end[list_ref_row] - reference$start[list_ref_row]

              return(dur)
            }
          })

        # look at detections matching 1 training selection at the time
        performance_list <-
          lapply(unique(labeled_detection$sound.files), function(z) {
            # get subset for that sound file
            sub_detec <-
              labeled_detection[labeled_detection$sound.files == z,]
            sub_ref <- reference[reference$sound.files == z,]

            # get row index in reference for detected sound events
            detected_reference_rows <-
              unique(na.omit(unlist(
                lapply(sub_detec$reference.row, function(x)
                  unlist(strsplit(
                    as.character(x), "-"
                  )))
              )))

            detected_reference_rows <-
              unique(sapply(grep(labeled_detection$detection.class, pattern = "^true.positive"), function(x) {
                ref_row <-
                  strsplit(labeled_detection$reference.row[x], split = "\\(")[[1]]
                ref_row <- ref_row[length(ref_row)]
                ref_row <- as.numeric(gsub("\\)", "", ref_row))
                return(ref_row)
              }))

            performance <- data.frame(
              sound.files = z,
              total.detections = nrow(sub_detec),
              true.positives = sum(
                grepl("^true.positive", x = sub_detec$detection.class)
              ),
              false.positives = sum(grepl(
                "false", sub_detec$detection.class
              )),
              false.negatives = sum(!sub_ref$..row.id %in% detected_reference_rows),
              split.positives = length(unique(unlist(
                lapply(sub_detec$reference.row[grepl("split)", sub_detec$detection.class)], function(x)
                  unlist(strsplit(x, "-")))
              ))),
              merged.positives = length(unique(unlist(
                lapply(sub_detec$reference.row[grepl("merged)", sub_detec$detection.class)], function(x)
                  unlist(strsplit(x, "-")))
              ))),
              mean.duration.true.positives = round(mean((sub_detec$end - sub_detec$start)[grep("true", sub_detec$detection.class)]
              ) * 1000, 0),
              mean.duration.false.positives = round(mean((sub_detec$end - sub_detec$start)[grep("false", sub_detec$detection.class)]
              ) * 1000, 0),
              mean.duration.false.negatives = round(mean((
                sub_ref$end - sub_ref$start
              )[!sub_ref$..row.id %in% detected_reference_rows]) * 1000, 0),
              overlap.to.true.positives = if (any(!is.na(sub_detec$overlap)))
                mean(sub_detec$overlap, na.rm = TRUE) else
                NA,
              proportional.duration.true.positives = mean(sub_detec$reference.duration, na.rm = TRUE) / mean((sub_ref$end - sub_ref$start)[sub_ref$..row.id %in% detected_reference_rows], na.rm = TRUE),
              stringsAsFactors = FALSE
            )

            # add duty cycle
            if (!is.null(path) & time.diagnostics) {
              # get file durations
              performance$duty.cycle <-
                sum((sub_detec$end - sub_detec$start), na.rm = TRUE) / warbleR::duration_sound_files(files = z, path = path)$duration
            }

            # add recall, precision and f1
            performance$recall <-
              performance$true.positives / nrow(sub_ref)
            performance$precision <-
              if (nrow(sub_detec) > 0 &
                  performance$true.positives > 0)
                (performance$true.positives / performance$total.detections) else
              0

            performance$f1.score <-
              2 * ((performance$precision * performance$recall) / (performance$precision + performance$recall)
              )

            # replace NaNs with NA
            for (i in 1:ncol(performance))
              if (is.nan(performance[, i]))
                performance[, i] <- NA

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
            total.detections = 0,
            true.positives = 0,
            false.positives = 0,
            false.negatives = sapply(setdiff(
              reference$sound.files,
              unique(labeled_detection$sound.files)
            ), function(x)
              sum(reference$sound.files == x)),
            split.positives = NA,
            merged.positives = NA,
            mean.duration.true.positives = NA,
            mean.duration.false.positives = NA,
            mean.duration.false.negatives = sapply(setdiff(
              reference$sound.files,
              unique(labeled_detection$sound.files)
            ), function(x)
              mean((reference$end - reference$start)[reference$sound.files == x])),
            overlap.to.true.positives = NA,
            proportional.duration.true.positives = NA,
            recall = 0,
            precision =  0,
            f1.score = 0,
            stringsAsFactors = FALSE
          )

          # add duty cycle
          if (!is.null(path) & time.diagnostics)
            no_detec$duty.cycle <- 0

          performance_list[[length(performance_list) + 1]] <- no_detec

        }
        # put in a single data frame
        performance_df <- do.call(rbind, performance_list)

      } else {
        performance_df <- data.frame(
          sound.files = unique(reference$sound.files),
          total.detections = 0,
          true.positives = 0,
          false.positives = 0,
          false.negatives = sapply(unique(reference$sound.files), function(x)
            sum(reference$sound.files == x)),
          split.positives = NA,
          merged.positives = NA,
          mean.duration.true.positives = NA,
          mean.duration.false.positives = NA,
          mean.duration.false.negatives = sapply(unique(reference$sound.files), function(x)
            mean(reference$end - reference$start)) * 1000,
          overlap.to.true.positives = NA,
          proportional.duration.true.positives = NA,
          recall = 0,
          precision = 0,
          f1.score = 0,
          stringsAsFactors = FALSE
        )
        # add duty cycle
        if (!is.null(path) & time.diagnostics)
          performance_df$duty.cycle <- 0

      }

      # sort columns
      performance_df <-
        performance_df[, na.omit(match(
          c(
            "sound.files",
            "total.detections",
            "true.positives",
            "false.positives",
            "false.negatives",
            "split.positives",
            "merged.positives",
            "mean.duration.true.positives",
            "mean.duration.false.positives",
            "mean.duration.false.negatives",
            "overlap.to.true.positives",
            "proportional.duration.true.positives",
            "duty.cycle",
            "recall",
            "precision",
            "f1.score"
          ),
          names(performance_df)
        ))]

      # summarize across sound files
      if (!by.sound.file)
        performance_df <-
        summarize_diagnostic(diagnostic = performance_df, time.diagnostics = time.diagnostics)

      # remove time diagnostics
      if (!time.diagnostics)
        performance_df <-
        performance_df[, grep(".duration.|duty", names(performance_df), invert = TRUE)]

      # fix row names
      rownames(performance_df) <- 1:nrow(performance_df)
    }
    return(performance_df)
  }
