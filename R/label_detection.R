#' @title Label detections from a sound event detection procedure
#'
#' @description \code{label_detection} labels the performance of a sound event detection procedure comparing the output selection table to a reference selection table
#' @usage label_detection(reference, detection, cores = 1, pb = TRUE)
#' @param reference Data frame or 'selection.table' (following the warbleR package format) with the reference selections (start and end of the sound events) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end". \strong{It must contain the reference selections that will be used for detection optimization}.
#' @param detection Data frame or 'selection.table' with the detections (start and end of the sound events) that will be compared against the 'reference' selections. Must contained at least the following columns: "sound.files", "selec", "start" and "end". It can contain data for additional sound files not found in 'references'. In this case the routine assumes that no sound events are found in those files, so detection from those files are all false positives.
#' @param cores Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return A data frame or selection table (if 'detection' was also a selection table, warbleR package's format, see \code{\link[warbleR]{selection_table}}) including the columns in 'detection' plus 3 additional columns:
#' \itemize{
#'  \item \code{detection.class}: indicates the class of each detection. Eight possible labels: 'true.positive', 'false.positive', 'true.positive (split)', 'true.positive (merged)',  'true.positive (split/merged)', 'false.positive (split)', 'false.positive (merged)' and 'galse.positive (split/merged)'.  See \code{\link{diagnose_detection}} for a description.
#'  \item \code{reference.row}: contains the index of the row in 'reference' of the reference sound event that is overlapped in time by the detection (not supplied for false positives).
#'  \item \code{overlap}: contains the proportion of the reference sound event that is overlapped in time by the detection (not supplied for false positives).
#'  }
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
#' label_detection(reference = lbh_reference,
#' detection = lbh_reference[c(1, 1:nrow(lbh_reference)), ])
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
#' Araya-Salas, M. (2021), ohun: diagnosing and optimizing automated sound event detection. R package version 0.1.0.
#' }
# last modification on jul-16-2021 (MAS)
label_detection <-
  function(reference,
           detection,
           cores = 1,
           pb = TRUE)
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

    # add row labels to reference to identify merged detections
    reference$..row.id <- 1:nrow(reference)

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
              as.data.frame(reference[reference$sound.files == z,])

            if (nrow(sub_detec) > 0) {
              # get index of reference sound events to which each detection overlaps
              true_positives_refer_row_id <-
                lapply(1:nrow(sub_detec), function(y) {
                  # defined as any detection that overlaps with the template selections
                  sub_ref$..row.id[(sub_ref$start >= sub_detec$start[y] &
                                      sub_ref$start < sub_detec$end[y]) |
                                     (sub_ref$end > sub_detec$start[y] &
                                        sub_ref$end <= sub_detec$end[y]) |
                                     (sub_ref$start <= sub_detec$start[y] &
                                        sub_ref$end >= sub_detec$end[y]) |
                                     (sub_ref$start >= sub_detec$start[y] &
                                        sub_ref$end  <= sub_detec$end[y])]

                })

              # convert to label
              sub_detec$detection.class <-
                sapply(true_positives_refer_row_id, function(x) {
                  # count how many times the reference selection overlapped (splits)
                  splits <- if (length(x) != 0)
                    max(table(unlist(true_positives_refer_row_id)[unlist(true_positives_refer_row_id) %in% x])) else
                    0

                  detection_class <- NA
                  if (length(x) == 0)
                    detection_class <- "false.positive"
                  if (length(x) == 1 &
                      splits == 1)
                    detection_class <- "true.positive"
                  if (length(x) == 1 &
                      splits > 1)
                    detection_class <- "true.positive (split)"
                  if (length(x) > 1 &
                      splits == 1)
                    detection_class <- "true.positive (merged)"
                  if (length(x) > 1 &
                      splits > 1)
                    detection_class <- "true.positive (split/merged)"

                  return(detection_class)
                })

              # add index of selection in reference
              sub_detec$reference.row <-
                sapply(true_positives_refer_row_id, function(x) {
                  if (length(x) == 0)
                    NA else
                    if (all(is.na(x)))
                      NA else
                    paste(x, collapse = "-")

                })

              # add overlap percentage
              sub_detec$overlap <- NA

              # only non-ambiguous true positives
              if (any(!grepl("-", sub_detec$reference.row) &
                      !is.na(sub_detec$reference.row)))
                sub_detec$overlap[!grepl("-", sub_detec$reference.row) &
                                    !is.na(sub_detec$reference.row)] <-
                sapply(which(
                  !grepl("-", sub_detec$reference.row) &
                    !is.na(sub_detec$reference.row)
                ), function(x) {
                  ovlp <-
                    min(sub_ref$end[sub_ref$..row.id == sub_detec$reference.row[x]] - sub_detec$start[x],
                        sub_detec$end[x] - sub_ref$start[sub_ref$..row.id == sub_detec$reference.row[x]]) / (sub_ref$end[sub_ref$..row.id == sub_detec$reference.row[x]] - sub_ref$start[sub_ref$..row.id == sub_detec$reference.row[x]])

                  return(ovlp)
                })

              # use maximum bipartite graph matching to solve ambiguous detections
              if (any(sapply(true_positives_refer_row_id, length) > 1)) {
                # get index of merges and splits
                ambiguous_positives <-
                  table(unlist(true_positives_refer_row_id))
                ambiguous_positives <-
                  unique(c(names(ambiguous_positives)[ambiguous_positives > 1],
                           unique(
                             unlist(true_positives_refer_row_id[sapply(true_positives_refer_row_id, length) > 1])
                           )))

                # get clean positives
                unambiguous_positives <-
                  names(table(unlist(true_positives_refer_row_id[sapply(true_positives_refer_row_id, length) == 1])))
                if (length(unambiguous_positives) > 0)
                  unambiguous_positives <-
                  unambiguous_positives[!unambiguous_positives %in% ambiguous_positives]

                ambiguous_detecs <-
                  which(sapply(true_positives_refer_row_id, function(x)
                    any(x %in% ambiguous_positives)))

                # get matrix to find maximum bipartite graph
                ovlp_mat <-
                  matrix(
                    rep(0, (
                      length(ambiguous_positives) * length(ambiguous_detecs)
                    )),
                    ncol = length(ambiguous_detecs),
                    dimnames = list(ambiguous_positives, ambiguous_detecs)
                  )

                # fill out with proportion of overlap for those in which overlap was already calculated
                for (i in ambiguous_detecs)
                  for (j in true_positives_refer_row_id[[i]]) {
                    ovlp_mat[as.character(j), as.character(i)] <-
                      if (!is.na(sub_detec$overlap[i]))
                        sub_detec$overlap[i] else
                      min(sub_ref$end[sub_ref$..row.id == j] - sub_detec$start[i],
                          sub_detec$end[i] - sub_ref$start[sub_ref$..row.id == j]) / (sub_ref$end[sub_ref$..row.id == j] - sub_ref$start[sub_ref$..row.id == j])
                  }

                # convert overlaps == 1 to 0.9999 (igraph ignores those equal to 1)
                org_ovlp_mat <- ovlp_mat
                ovlp_mat[ovlp_mat > 1] <- 0.99999


                # convert to graph
                ovlp_graph <-
                  igraph::graph_from_incidence_matrix(ovlp_mat)

                #convert to data frame
                df_ovlp_graph <- igraph::as_data_frame(ovlp_graph)

                # add amount of overlap as weights
                igraph::E(ovlp_graph)$weight <-
                  sapply(1:nrow(df_ovlp_graph), function(x)
                    ovlp_mat[df_ovlp_graph$from[x], df_ovlp_graph$to[x]])

                # plot just to troubleshoot
                # plot.igraph(ovlp_graph, layout = layout_as_bipartite,
                #             vertex.color=c("green","cyan")[V(ovlp_graph)$type+1], edge.width=(3*E(g)$weight), vertex.size = 40)

                # get maximum bipartite graph
                bigraph_results <-
                  igraph::max_bipartite_match(ovlp_graph)$matching

                # keep only detection results
                bigraph_results <-
                  bigraph_results[-c(1:length(ambiguous_positives))]

                # get those that remain true positives
                bigraph_true_positives <-
                  names(bigraph_results)[!is.na(bigraph_results)]

                # get those that change to false positives
                bigraph_false_positives <-
                  names(bigraph_results)[is.na(bigraph_results)]

                # fix labels on sub detec
                sub_detec$reference.row[as.numeric(bigraph_true_positives)] <-
                  paste(sub_detec$reference.row[as.numeric(bigraph_true_positives)],
                        " (",
                        bigraph_results[bigraph_true_positives],
                        ")",
                        sep = "")

                # add selected matching reference to unambiguous detections
                sub_detec$reference.row[!1:nrow(sub_detec) %in% ambiguous_detecs] <-
                  paste(sub_detec$reference.row[!1:nrow(sub_detec) %in% ambiguous_detecs],
                        " (",
                        sub_detec$reference.row[!1:nrow(sub_detec) %in% ambiguous_detecs],
                        ")",
                        sep = "")


                # add overlaps to those that were selected as true positives
                sub_detec$overlap[as.numeric(bigraph_true_positives)] <-
                  sapply(bigraph_true_positives, function(x)
                    org_ovlp_mat[df_ovlp_graph$from[df_ovlp_graph$to == x &
                                                      df_ovlp_graph$from == bigraph_results[x]], x])

                # re label those that turned out to be false positivies
                if (length(bigraph_false_positives) > 0)
                  sub_detec$detection.class[as.numeric(bigraph_false_positives)] <-
                  gsub(pattern = "true",
                       replacement = "false",
                       sub_detec$detection.class[as.numeric(bigraph_false_positives)])
              } else
                # add matching reference in parenthesis
                sub_detec$reference.row[!is.na(sub_detec$reference.row)] <-
                paste(sub_detec$reference.row[!is.na(sub_detec$reference.row)],
                      " (",
                      sub_detec$reference.row[!is.na(sub_detec$reference.row)],
                      ")",
                      sep = "")

              # make 1 overlaps higher than 1
              if (any(!is.na(sub_detec$overlap))) {
                sub_detec$overlap[sub_detec$overlap > 1] <- 1
                sub_detec$overlap <- round(sub_detec$overlap, 2)
              }


            }
          } else {
            sub_detec$detection.class <- "false.positive"
            sub_detec$reference.row <- NA
            sub_detec$overlap <- NA
          }

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

    return(labeled_detections)
  }
