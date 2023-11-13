#' @title Label detections from a sound event detection procedure
#'
#' @description \code{label_detection} labels the performance of a sound event detection procedure comparing the output selection table to a reference selection table
#' @usage label_detection(reference, detection, cores = 1, pb = TRUE, min.overlap = 0.5,
#'  by = NULL)
#' @param reference Data frame or 'selection.table' (following the warbleR package format) with the reference selections (start and end of the sound events) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end". \strong{It must contain the reference selections that will be used for detection optimization}.
#' @param detection Data frame or 'selection.table' with the detections (start and end of the sound events) that will be compared against the 'reference' selections. Must contained at least the following columns: "sound.files", "selec", "start" and "end". It can contain data for additional sound files not found in 'references'. In this case the routine assumes that no sound events are found in those files, so detection from those files are all false positives.
#' @param cores Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param min.overlap Numeric. Controls the minimum amount of overlap required for a detection and a reference sound for it to be counted as true positive. Default is 0.5. Overlap is measured as intersection over union.
#' @param by Character vector with the name of a categorical column in 'reference' for running a stratified. Labels will be returned separated for each level in 'by'. Default is \code{NULL}.
#' @return A data frame or selection table (if 'detection' was also a selection table, warbleR package's format, see \code{\link[warbleR]{selection_table}}) including three additional columns, 'detection.class', which indicates the class of each detection, 'reference' which identifies the event in the 'reference' table that was detected  and 'overlap' which refers to the amount overlap to the reference sound. See \code{\link{diagnose_detection}} for a description of the labels used in 'detection.class'. The output data frame also contains an additional data frame with the overlap for each pair of overlapping detection/reference.  Overlap is measured as intersection over union.
#' @export
#' @name label_detection
#' @details The function identifies the rows in the output of a detection routine as true or false positives. This is achieved by comparing the data frame to a reference selection table in which all sound events of interest have been selected.
#' @examples {
#'   # load data
#'   data("lbh_reference")
#'
#'   # an extra one in detection (1 false positive)
#'   label_detection(reference = lbh_reference[-1, ], detection = lbh_reference)
#'
#'   # missing one in detection (all true positives)
#'   label_detection(reference = lbh_reference, detection = lbh_reference[-1, ])
#'
#'   # perfect detection (all true positives)
#'   label_detection(reference = lbh_reference, detection = lbh_reference)
#'
#'   # and extra sound file in reference (all true positives)
#'   label_detection(
#'     reference = lbh_reference, detection =
#'       lbh_reference[lbh_reference$sound.files != "lbh1.wav", ]
#'   )
#'
#'   # and extra sound file in detection (some false positives)
#'   label_detection(
#'     reference =
#'       lbh_reference[lbh_reference$sound.files != "lbh1.wav", ],
#'     detection = lbh_reference
#'   )
#'
#'   # duplicate 1 detection row (to get 2 splits)
#'   detec <- lbh_reference[c(1, seq_len(nrow(lbh_reference))), ]
#'   detec$selec[1] <- 1.2
#'   label_detection(
#'     reference = lbh_reference,
#'     detection = detec
#'   )
#'
#'   # merge 2 detections (to get split and merge)
#'   Y <- lbh_reference
#'   Y$end[1] <- 1.2
#'   label_detection(reference = lbh_reference, detection = Y)
#'
#'   # remove split to get only merge
#'   Y <- Y[-2, ]
#'   label_detection(reference = lbh_reference, detection = Y)
#' }
#' @seealso \code{\link{diagnose_detection}}, \code{\link{summarize_diagnostic}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references {
#' Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C., Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. 2022. ohun: an R package for diagnosing and optimizing automatic sound event detection. BioRxiv, 2022.12.13.520253. https://doi.org/10.1101/2022.12.13.520253
#' }

label_detection <-
  function(reference,
           detection,
           cores = 1,
           pb = TRUE,
           min.overlap = 0.5,
           by = NULL) {
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

    # do it by
    # run the function for each subset split by "by"
    if (!is.null(by)) {
      split_det <- split(x = detection, f = detection[, by])

      split_labeled <-
        warbleR:::pblapply_wrblr_int(
          X = seq_len(length(split_det)),
          cl = 1,
          pbar = pb,
          FUN = function(x) {
            by_lab <-
              label_detection(
                reference = reference,
                detection = split_det[[x]],
                pb = FALSE,
                cores = cores,
                min.overlap = min.overlap
              )

            return(by_lab)
          }
        )

      labeled_detections <- do.call(rbind, split_labeled)

      # fix call
      if (warbleR::is_selection_table(detection)) {
        attributes(labeled_detections)$call <- base::match.call()
      }
    } else {
      if (any(!complete.cases(detection[, c("start", "end")]))) {
        detection <- detection[complete.cases(detection[, c("start", "end")]), ]
        warning2("Rows in 'detection' with missing values in start and/or end were removed")
      }

      # set clusters for windows OS
      if (Sys.info()[1] == "Windows" & cores > 1) {
        cl <- parallel::makeCluster(cores)
      } else {
        cl <- cores
      }

      # look at detections matching 1 training selection at the time
      labeled_detections_list <-
        warbleR:::pblapply_wrblr_int(
          pbar = pb,
          cl = cl,
          X = unique(detection$sound.files),
          FUN = function(z) {
            # get subset from detection for that sound file
            sub_detec <-
              as.data.frame(detection[detection$sound.files == z, ])

            # if sound file is found in references
            if (any(reference$sound.files == z)) {
              # get subset from template for that sound file
              sub_ref <-
                as.data.frame(reference[reference$sound.files == z, ])

              ovlp_events <- overlapping_detections(reference = sub_ref, detection = sub_detec)

              overlap_iou <- pairs_iou(df = ovlp_events, detection = sub_detec, reference = sub_ref)

              if (nrow(overlap_iou) > 0) {
                # filter detections below minimum overlap
                overlap_iou <- overlap_iou[overlap_iou$IoU > min.overlap, ]

                sub_detec$detection.class <- vapply(paste(sub_detec$sound.files, sub_detec$selec, sep = "-"), function(y) {
                  # if no reference overlap is a false positive
                  if (sum(overlap_iou$detection.id == y) == 0) detection.class <- "false.positive"

                  # if only one overlapping reference is a true positive
                  if (sum(overlap_iou$detection.id == y) == 1) detection.class <- "true.positive"

                  # if more than one overlapping reference is a merged true positive
                  if (sum(overlap_iou$detection.id == y) > 1) detection.class <- "true.positive (merged)"

                  # if 1 reference overlaps and that reference overlaps with other detections
                  if (sum(overlap_iou$detection.id == y) == 1 & any(overlap_iou$reference.id[overlap_iou$detection.id != y] %in% overlap_iou$reference.id[overlap_iou$detection.id == y])) detection.class <- "true.positive (split)"

                  if (sum(overlap_iou$detection.id == y) > 1 & any(overlap_iou$reference.id[overlap_iou$detection.id != y] %in% overlap_iou$reference.id[overlap_iou$detection.id == y])) detection.class <- "true.positive (split/merged)"

                  return(detection.class)
                }, FUN.VALUE = character(1))

                # use maximum bipartite graph matching to solve ambiguous detections
                if (any(grepl("split|merge", sub_detec$detection.class))) {
                  ambiguous_detec <- sub_detec[grepl("split|merge", sub_detec$detection.class), ]
                  ambiguous_detec$id <- paste(ambiguous_detec$sound.files, ambiguous_detec$selec, sep = "-")
                  sub_ref$id <- paste(sub_ref$sound.files, sub_ref$selec, sep = "-")

                  ambiguous_overlaps <- overlap_iou[overlap_iou$detection.id %in% ambiguous_detec$id, ]

                  ambiguous_ref <- sub_ref[sub_ref$id %in% ambiguous_overlaps$reference.id, ]

                  # get matrix to find maximum bipartite graph
                  ovlp_mat <-
                    matrix(
                      rep(0, (
                        nrow(ambiguous_detec) * nrow(ambiguous_ref)
                      )),
                      ncol = nrow(ambiguous_ref),
                      dimnames = list(ambiguous_detec$id, ambiguous_ref$id)
                    )
                  #
                  # fill out with proportion of overlap for those in which overlap was already calculated
                  for (i in seq_len(nrow(ambiguous_overlaps))) {
                    ovlp_mat[rownames(ovlp_mat) == ambiguous_overlaps$detection.id[i], colnames(ovlp_mat) == ambiguous_overlaps$reference.id[i]] <- ambiguous_overlaps$IoU[i]
                  }

                  # convert overlaps == 1 to 0.9999 (igraph ignores those equal to 1)
                  # org_ovlp_mat <- ovlp_mat
                  ovlp_mat[ovlp_mat >= 1] <- 0.99999

                  # convert to graph
                  ovlp_graph <-
                    igraph::graph_from_incidence_matrix(ovlp_mat)

                  # convert to data frame
                  df_ovlp_graph <- igraph::as_data_frame(ovlp_graph)

                  # add amount of overlap as weights
                  igraph::E(ovlp_graph)$weight <-
                    vapply(seq_len(nrow(df_ovlp_graph)), function(x) {
                      ovlp_mat[df_ovlp_graph$from[x], df_ovlp_graph$to[x]]
                    }, FUN.VALUE = numeric(1))
                  #
                  #   # plot just to troubleshoot
                  #   # plot.igraph(ovlp_graph, layout = layout_as_bipartite,
                  #   #             vertex.color=c("green","cyan")[V(ovlp_graph)$type+1], edge.width=(3*E(g)$weight), vertex.size = 40)
                  #
                  #   # get maximum bipartite graph
                  bigraph_results <-
                    igraph::max_bipartite_match(ovlp_graph)$matching

                  # keep only detection results
                  bigraph_results <-
                    bigraph_results[1:nrow(ovlp_mat)]

                  # keep only true positive overlaps
                  overlap_iou <- overlap_iou[paste(overlap_iou$detection.id, overlap_iou$reference.id) %in% paste(names(bigraph_results), bigraph_results) | !overlap_iou$detection.id %in% ambiguous_detec$id, ]

                  # get those that change to false positives
                  bigraph_false_positives <-
                    names(bigraph_results)[is.na(bigraph_results)]

                  if (length(bigraph_false_positives) > 0) {
                    sub_detec$detection.class[paste(sub_detec$sound.files, sub_detec$selec, sep = "-") %in% bigraph_false_positives] <- gsub("true.positive", "false.positive", sub_detec$detection.class[paste(sub_detec$sound.files, sub_detec$selec, sep = "-") %in% bigraph_false_positives])
                  }
                }
              }
            } else {
              overlap_iou <- data.frame(sound.files = vector(), detection.id = vector(), reference.id = vector(), IoU = vector())
            }

            if (nrow(overlap_iou) == 0) {
              sub_detec$detection.class <- "false.positive"
            }


            if (any(grepl("true.positive", sub_detec$detection.class))) {
              sub_detec$overlap <- vapply(seq_len(nrow(sub_detec)),
                function(x) {
                  ovlp <- overlap_iou$IoU[overlap_iou$detection.id == paste(sub_detec$sound.files[x], sub_detec$selec[x], sep = "-")]
                  if (length(ovlp) == 0) ovlp <- NA
                  return(ovlp)
                },
                FUN.VALUE = numeric(1)
              )
              sub_detec$reference <- vapply(seq_len(nrow(sub_detec)),
                function(x) {
                  ref <- overlap_iou$reference.id[overlap_iou$detection.id == paste(sub_detec$sound.files[x], sub_detec$selec[x], sep = "-")]
                  if (length(ref) == 0) ref <- NA
                  return(as.character(ref))
                },
                FUN.VALUE = character(1)
              )
            } else {
              sub_detec$overlap <- NA
              sub_detec$reference <- NA
            }

            # include overlaps as attributes
            attributes(sub_detec)$overlaps <- overlap_iou

            return(sub_detec)
          }
        )

      # put results in a single data frame
      labeled_detections <- do.call(rbind, labeled_detections_list)


      # convert to selection table
      if (warbleR::is_selection_table(detection)) {
        detection$detection.class <- labeled_detections$detection.class
        detection$reference.row <- labeled_detections$reference.row
        detection$reference <- labeled_detections$reference
        detection$overlap <- labeled_detections$overlap

        # overwrite labeled_detections
        labeled_detections <- detection

        # fix call
        attributes(labeled_detections)$call <- base::match.call()
      }

      rownames(labeled_detections) <- 1:nrow(labeled_detections)

      attributes(labeled_detections)$overlaps <- do.call(rbind, lapply(labeled_detections_list, function(x) attributes(x)$overlaps))
    }
    return(labeled_detections)
  }
