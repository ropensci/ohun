#' @title Evaluate the performance of a signal detection procedure
#'
#' @description \code{label_detection} evaluates the performance of a signal detection procedure comparing the output selection table to a reference selection table
#' @usage label_detection(reference, detection, parallel = 1, pb = TRUE)
#' @param reference Data frame or 'selection.table' (following the warbleR package format) with the reference selections (start and end of the signals) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end". \strong{It must contain the reference selections that will be used for detection optimization}.
#' @param detection Data frame or 'selection.table' with the detections (start and end of the signals) that will be compared against the 'reference' selections. Must contained at least the following columns: "sound.files", "selec", "start" and "end". It can contain data for additional sound files not found in 'references'. In this case the routine assumes that no signals are found in those files, so detection from those files are all false positives.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return A data frame including the columns in 'detection' plust 2 additional columns:
#' \itemize{
#'  \item \code{detection.class}: indicates the class of each detection. Five possible labels: 'true.positive', 'false.positive', 'true.positive (split)', 'true.positive (merged)' and 'true.positive (split/merged)'.  See \code{\link{diagnose_detection}} for a description.
#'  \item \code{reference.row}: contains the index of the row in 'reference' that corresponds to the detected signal (only supplied for true positives).
#'  \item \code{overlap}: contains the proportion of the reference signal that is overlapped in time by the detection (only supplied for true positives).
#'  }
#' @export
#' @name label_detection
#' @details The function identifies the rows in the output of a detection routine as true or false positives. This is achieved by comparing the data frame to a reference selection table in which all signals of interest have been selected.
#' @examples {
#' # an extra one in detection (1 false positive)
#' label_detection(reference = lbh_selec_reference[-1, ], detection = lbh_selec_reference)
#'
#' # missing one in detection (all true positives)
#' label_detection(reference = lbh_selec_reference, detection = lbh_selec_reference[-1, ])
#'
#' # perfect detection (all true positives)
#' label_detection(reference = lbh_selec_reference, detection = lbh_selec_reference)
#'
#' # and extra sound file in reference (all true positives)
#' label_detection(reference = lbh_selec_reference, detection =
#' lbh_selec_reference[lbh_selec_reference$sound.files != "Phae.long1.wav", ])
#'
#' # and extra sound file in detection (some false positives)
#' label_detection(reference =
#' lbh_selec_reference[lbh_selec_reference$sound.files != "Phae.long1.wav", ],
#' detection = lbh_selec_reference)
#'
#' # duplicate 1 detection row (to get 2 splits)
#' label_detection(reference = lbh_selec_reference,
#' detection = lbh_selec_reference[c(1, 1:nrow(lbh_selec_reference)), ])
#'
#' # merge 2 detections (to get split and merge)
#' Y <- lbh_selec_reference
#' Y$end[1] <- 1.2
#' label_detection(reference = lbh_selec_reference, detection = Y)
#'
#' # remove split to get only merge
#' Y <- Y[-2, ]
#' label_detection(reference = lbh_selec_reference, detection = Y)
#' }
#' @seealso \code{\link{diagnose_detection}}, \code{\link{summarize_diagnostic}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references {
#' Araya-Salas, M. (2021), ohun: automatic detection of acoustic signals. R package version 0.1.0.
#' }
# last modification on jul-16-2021 (MAS)
label_detection <- function(reference, detection, parallel = 1, pb = TRUE)
{
  if (is_extended_selection_table(reference)) stop("This function cannot take extended selection tables ('reference' argument)")

  #if reference is not a data frame
  if (!any(is.data.frame(reference), is_selection_table(reference)))
    stop("'reference' is not of a class 'data.frame' or 'selection_table'")

  #if reference is not a data frame
  if (!any(is.data.frame(detection), is_selection_table(detection)))
    stop("'detection' is not of a class 'data.frame' or 'selection_table'")

  #check if all columns are found in reference
  if (any(!(c(
    "sound.files", "selec", "start", "end"
  ) %in% colnames(reference))))
    stop(paste(paste(
      c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec",
                                                    "start", "end") %in% colnames(reference))], collapse =
        ", "
    ), "column(s) not found in 'reference'"))

  #check if all columns are found in detection
  if (any(!(c(
    "sound.files", "selec", "start", "end"
  ) %in% colnames(detection))))
    stop(paste(paste(
      c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec",
                                                    "start", "end") %in% colnames(detection))], collapse =
        ", "
    ), "column(s) not found in 'detection'"))

  #if there are NAs in start or end stop (reference)
  if (any(is.na(c(reference$end, reference$start))))
    stop("NAs found in start and/or end columns")

  #if any start higher than end stop
  if (any(reference$end - reference$start <= 0))
    stop(paste(
      "Start is higher than or equal to end in",
      length(which(reference$end - reference$start <= 0)),
      "case(s) in 'reference'"
    ))


  # add row labels to reference to identify merged detections
  reference$..row.id <- 1:nrow(reference)

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makeCluster(parallel) else
      cl <- parallel

    # look at detections matching 1 training selection at the time
      labeled_detections_list <- warbleR:::pblapply_wrblr_int(pbar = pb, cl = cl, X =  unique(detection$sound.files), FUN = function(z){

        # get subset from detection for that sound file
        sub_detec <- detection[detection$sound.files == z, ]

        # if sound file is found in references
        if (any(reference$sound.files == z))
  {
        # get subset from template for that sound file
        sub_ref <- reference[reference$sound.files == z, ]

        if (nrow(sub_detec) > 0){

        # get index of reference signals to which each detection overlaps
        true_positives_refer_row_id <- lapply(1:nrow(sub_detec), function(y){

          # defined as any detection that overlaps with the template selections
          sub_ref$..row.id[(sub_ref$start >= sub_detec$start[y] & sub_ref$start < sub_detec$end[y]) |
                               (sub_ref$end > sub_detec$start[y] & sub_ref$end <= sub_detec$end[y]) |
                               (sub_ref$start <= sub_detec$start[y] & sub_ref$end >= sub_detec$end[y]) |
                               (sub_ref$start >= sub_detec$start[y] & sub_ref$end  <= sub_detec$end[y])]

          })

        # conver to label
        sub_detec$detection.class <- sapply(true_positives_refer_row_id, function(x){

          # count how many times the reference selection overlapped (splits)
          splits <- if (length(x) != 0)
max(table(unlist(true_positives_refer_row_id)[unlist(true_positives_refer_row_id) %in% x]))  else 0

          if (length(x) == 0) detection_class <- "false.positive"
          if (length(x) == 1 & splits == 1) detection_class <- "true.positive"
          if (length(x) == 1 & splits > 1) detection_class <- "true.positive (split)"
          if (length(x) > 1 & splits == 1) detection_class <- "true.positive (merged)"
          if (length(x) > 1 & splits > 1) detection_class <- "true.positive (split/merged)"

          return(detection_class)
            })

        # add index of selection in reference
        sub_detec$reference.row <- sapply(true_positives_refer_row_id, function(x){

          if (length(x) == 0) NA else paste(x, collapse = "-")

        })

        # add overlap percentage
        sub_detec$overlap <- NA

          # only non-ambiguous true positives
        sub_detec$overlap[!grepl("-", sub_detec$reference.row) & !is.na(sub_detec$reference.row)] <- sapply(which(!grepl("-", sub_detec$reference.row) & !is.na(sub_detec$reference.row)), function(x){

        ovlp <- min(sub_ref$end[sub_ref$..row.id == sub_detec$reference.row[x]] - sub_detec$start[x], sub_detec$end[x] - sub_ref$start[sub_ref$..row.id == sub_detec$reference.row[x]]) / (sub_ref$end[sub_ref$..row.id == sub_detec$reference.row[x]] - sub_ref$start[sub_ref$..row.id == sub_detec$reference.row[x]])

        if (ovlp > 1) ovlp <- 1
        return(ovlp)
        })

          }
        } else{
          sub_detec$detection.class <- "false.positive"
          sub_detec$reference.row <- NA
          sub_detec$overlap <- NA
}
    return(sub_detec)
    })

    # put results in a single data frame
    labeled_detections <- do.call(rbind, labeled_detections_list)

    # if (is.list(labeled_detections$overlap))
      # labeled_detections$overlap <- unlist(labeled_detections$overlap)

  return(labeled_detections)
}
