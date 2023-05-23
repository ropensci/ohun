#' @title Label detections from a sound event detection procedure
#'
#' @description \code{label_detection} labels the performance of a sound event detection procedure comparing the output selection table to a reference selection table
#' @usage label_detection(reference, detection, cores = 1, pb = TRUE, min.overlap = 0.5)
#' @param reference Data frame or 'selection.table' (following the warbleR package format) with the reference selections (start and end of the sound events) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end". \strong{It must contain the reference selections that will be used for detection optimization}.
#' @param detection Data frame or 'selection.table' with the detections (start and end of the sound events) that will be compared against the 'reference' selections. Must contained at least the following columns: "sound.files", "selec", "start" and "end". It can contain data for additional sound files not found in 'references'. In this case the routine assumes that no sound events are found in those files, so detection from those files are all false positives.
#' @param cores Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param min.overlap Numeric. Controls the minimum amount of overlap required for a detection and a reference sound for it to be counted as true positive. Default is 0.5. Overlap is measured as intersection over union.
#' @return A data frame or selection table (if 'detection' was also a selection table, warbleR package's format, see \code{\link[warbleR]{selection_table}}) including two additional columns, 'detection.class', which indicates the class of each detection and 'overlap' which refers to the amount overlap to the reference sound. See \code{\link{diagnose_detection}} for a description of the labels used in 'detection.class'. The output data frame also contains an additional data frame with the overlap for each pair of overlapping detection/reference.  Overlap is measured as intersection over union.
#' @export
#' @name label_detection
#' @details The function identifies the rows in the output of a detection routine as true or false positives. This is achieved by comparing the data frame to a reference selection table in which all sound events of interest have been selected.
#' @examples {
#' # load data
#' data("lbh_reference")
#'
#' # an extra one in detection (1 false positive)
#' label_detection(reference = lbh_reference[-1, ], detection = lbh_reference)
#'
#' # missing one in detection (all true positives)
#' label_detection(reference = lbh_reference, detection = lbh_reference[-1, ])
#'
#' # perfect detection (all true positives)
#' label_detection(reference = lbh_reference, detection = lbh_reference)
#'
#' # and extra sound file in reference (all true positives)
#' label_detection(reference = lbh_reference, detection =
#' lbh_reference[lbh_reference$sound.files != "lbh1.wav", ])
#'
#' # and extra sound file in detection (some false positives)
#' label_detection(reference =
#' lbh_reference[lbh_reference$sound.files != "lbh1.wav", ],
#' detection = lbh_reference)
#'
#' # duplicate 1 detection row (to get 2 splits)
#' detec <- lbh_reference[c(1, seq_len(nrow(lbh_reference))), ]
#' detec$selec[1] <- 1.2
#' label_detection(reference = lbh_reference,
#' detection = detec)
#'
#' # merge 2 detections (to get split and merge)
#' Y <- lbh_reference
#' Y$end[1] <- 1.2
#' label_detection(reference = lbh_reference, detection = Y)
#'
#' # remove split to get only merge
#' Y <- Y[-2, ]
#' label_detection(reference = lbh_reference, detection = Y)
#' }
#' @seealso \code{\link{diagnose_detection}}, \code{\link{summarize_diagnostic}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references {
#' Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C., Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. 2022. ohun: an R package for diagnosing and optimizing automatic sound event detection. BioRxiv, 2022.12.13.520253. https://doi.org/10.1101/2022.12.13.520253
#' }
# last modification on jul-16-2021 (MAS)
label_detection <-
  function(reference,
           detection,
           cores = 1,
           pb = TRUE,
           min.overlap = 0.5)
  {
    if (is_extended_selection_table(reference))
      stop2("This function cannot take extended selection tables ('reference' argument)")

    if (is_extended_selection_table(detection))
      stop2("This function cannot take extended selection tables ('detection' argument)")

    if (any(!complete.cases(detection[, c("start", "end")]))){
      detection <- detection[complete.cases(detection[, c("start", "end")]),]
      warning2("Rows in 'detection' with missing values in start and/or end were removed")}

    #if reference is not a data frame
    if (!any(is.data.frame(reference), is_selection_table(reference)))
      stop2("'reference' is not of a class 'data.frame' or 'selection_table'")

    #if reference is not a data frame
    if (!any(is.data.frame(detection), is_selection_table(detection)))
      stop2("'detection' is not of a class 'data.frame' or 'selection_table'")

    #check if all columns are found in reference
    if (any(!(
      c("sound.files", "selec", "start", "end") %in% colnames(reference)
    )))
      stop2(paste(paste(
        c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec",
                                                      "start", "end") %in% colnames(reference))], collapse =
          ", "
      ), "column(s) not found in 'reference'"))

    # no duplicated selection labels
    if (anyDuplicated(paste(detection$sound.files, detection$selec)) > 0)
      stop2("Duplicated 'selec' labels within at least one sound file in 'detection'")

    if (anyDuplicated(paste(reference$sound.files, reference$selec)) > 0)
      stop2("Duplicated 'selec' labels within at least one sound file in 'reference'")

        #check if all columns are found in detection
    if (any(!(
      c("sound.files", "selec", "start", "end") %in% colnames(detection)
    )))
      stop2(paste(paste(
        c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec",
                                                      "start", "end") %in% colnames(detection))], collapse =
          ", "
      ), "column(s) not found in 'detection'"))

    #if there are NAs in start or end stop (reference)
    if (any(is.na(c(reference$end, reference$start))))
      stop2("NAs found in start and/or end columns")

    #if any start higher than end stop
    if (any(reference$end - reference$start <= 0))
      stop2(paste(
        "Start is higher than or equal to end in",
        length(which(reference$end - reference$start <= 0)),
        "case(s) in 'reference'"
      ))

    # set clusters for windows OS
    if (Sys.info()[1] == "Windows" & cores > 1)
      cl <- parallel::makeCluster(cores) else
      cl <- cores

    # look at detections matching 1 training selection at the time
    labeled_detections_list <-
      warbleR:::pblapply_wrblr_int(
        pbar = pb,
        cl = cl,
        X = unique(detection$sound.files),
        FUN = function(z) {

          # get subset from detection for that sound file
          sub_detec <-
            as.data.frame(detection[detection$sound.files == z,])

          # if sound file is found in references
          if (any(reference$sound.files == z))
          {
            # get subset from template for that sound file
            sub_ref <-
              as.data.frame(reference[reference$sound.files == z, ])

            ovlp_events <- overlapping_detections(reference = sub_ref, detection = sub_detec)
            overlap_iou <- pairs_iou(df = ovlp_events, detection = sub_detec, reference = sub_ref)

            # filter detections below minimum overlap
            overlap_iou <- overlap_iou[overlap_iou$IoU > min.overlap, ]

            sub_detec$detection.class <- vapply(paste(sub_detec$sound.files, sub_detec$selec, sep = "-"), function(y){
              # if no reference overlap is a false positive
              if (sum(overlap_iou$detection.id == y) == 0) detection.class <- "false.positive"

              # if only one overlapping reference is a true positive
              if (sum(overlap_iou$detection.id == y) == 1) detection.class <-  "true.positive"

              # if more than one overlapping reference is a merged true positive
              if (sum(overlap_iou$detection.id == y) > 1) detection.class <-  "true.positive (merged)"

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

              ambiguous_overlaps <- overlap_iou[overlap_iou$detection.id %in%  ambiguous_detec$id, ]

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
                for (i in seq_len(nrow(ambiguous_overlaps)))
                    ovlp_mat[rownames(ovlp_mat) == ambiguous_overlaps$detection.id[i], colnames(ovlp_mat) ==  ambiguous_overlaps$reference.id[i]] <- ambiguous_overlaps$IoU[i]

                # convert overlaps == 1 to 0.9999 (igraph ignores those equal to 1)
                # org_ovlp_mat <- ovlp_mat
                ovlp_mat[ovlp_mat >= 1] <- 0.99999

                # convert to graph
                ovlp_graph <-
                  igraph::graph_from_incidence_matrix(ovlp_mat)

                #convert to data frame
                df_ovlp_graph <- igraph::as_data_frame(ovlp_graph)

                # add amount of overlap as weights
                igraph::E(ovlp_graph)$weight <-
                  vapply(seq_len(nrow(df_ovlp_graph)), function(x)
                    ovlp_mat[df_ovlp_graph$from[x], df_ovlp_graph$to[x]], FUN.VALUE = numeric(1))
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

                if (length(bigraph_false_positives) > 0)
                  sub_detec$detection.class[paste(sub_detec$sound.files, sub_detec$selec, sep = "-") %in% bigraph_false_positives] <- gsub("true.positive", "false.positive", sub_detec$detection.class[paste(sub_detec$sound.files, sub_detec$selec, sep = "-") %in% bigraph_false_positives])
                }
              } else{
                sub_detec$detection.class <- "false.positive"

                overlap_iou <- data.frame(sound.files = vector(), detection.id  = vector(), reference.id  = vector(), IoU  = vector())
                }

              if (any(grepl("true.positive", sub_detec$detection.class))) {

                sub_detec$overlap <- vapply(seq_len(nrow(sub_detec)),
                                            function(x){
                  ovlp <- overlap_iou$IoU[overlap_iou$detection.id == paste(sub_detec$sound.files[x], sub_detec$selec[x], sep = "-")]
                  if (length(ovlp) == 0) ovlp <- NA
                  return(ovlp)
                  },
                  FUN.VALUE = numeric(1))
              } else  sub_detec$overlap <- NA

          # include overlaps as attributes
          attributes(sub_detec)$overlaps <- overlap_iou

          return(sub_detec)
        }
      )

    # put results in a single data frame
    labeled_detections <- do.call(rbind, labeled_detections_list)


    # convert to selection table
    if (is_selection_table(detection)) {
      detection$detection.class <- labeled_detections$detection.class
      detection$reference.row <- labeled_detections$reference.row
      detection$overlap <- labeled_detections$overlap

      # overwrite labeled_detections
      labeled_detections <- detection

      # fix call
      attributes(labeled_detections)$call <- base::match.call()
    }

    rownames(labeled_detections) <- 1:nrow(labeled_detections)

    attributes(labeled_detections)$overlaps <- do.call(rbind, lapply(labeled_detections_list, function(x) attributes(x)$overlaps))


    return(labeled_detections)
  }
