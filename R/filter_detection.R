#' @title  Remove ambiguous detections
#'
#' @description \code{filter_detection} removes ambiguous detections (split and merged detections)
#' @usage filter_detection(detection, by = "overlap", filter = "max", parallel = 1, pb = TRUE)
#' @param detection Data frame with the output of \code{\link{label_detection}} containing the start and end of the signals. Must contained at least the following columns: "sound.files", "selec", "start", "end". It must also contained the column indicated in the 'by' argument.
#' @param by Character vector of length 1 indicating a column in 'detection' that will be used to filter delections. Must refer to a numeric column. Default is 'overlap', which is return by \code{\link{label_detection}}.
#' @param filter Character vector of length 1 indicating the criterium used to filter the column refer to by the 'by' argument. Current options are 'max' (maximum) and 'min' (minimum). Default is 'max'.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return An object of class 'envelopes'.
#' @export
#' @name filter_detection
#' @details This function removes ambiguous detections (split or merged detections, see \code{\link{diagnose_detection}}) keeping only the one that maximizes a criterium given by 'filter'. By default it keeps the detection with the highest overlap to the reference signal. It works on the output of \code{\link{label_detection}}.
#'
#' @examples {
#' # load example data
#' data(list = "Phae.long1")
#'
#' # save sound files
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"))
#'
#' # template for the first sound file in 'lbh_selec_reference'
#' templ1 <- lbh_selec_reference[1, ]
#'
#' # generate template correlations
#' tc <- template_correlator(templates = templ1, path = tempdir(),
#' files = "Phae.long1.wav")
#'
#' # template detection
#' td <- template_detector(template.correlations = tc, threshold = 0.12)
#'
#' # this detection generates 2 split positives
#' diagnose_detection(reference = lbh_selec_reference[lbh_selec_reference == "Phae.long1.wav", ],
#' detection = td)
#'
#' # label detection
#' ltd <- label_detection(reference = lbh_selec_reference[lbh_selec_reference == "Phae.long1.wav", ],
#' detection = td)
#'
#' # now they can be filter to keep the detection with the highest score for each split
#' ftd <- filter_detection(ltd)
#'
#' # splits must be 0
#' diagnose_detection(reference = lbh_selec_reference[lbh_selec_reference == "Phae.long1.wav", ],
#' detection = ftd)
#' }
#'
#' @references {
#'#' Araya-Salas, M. (2021), ohun: automatic detection of acoustic signals. R package version 0.1.0.
#' }
#' @seealso \code{\link{label_detection}}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}).
#last modification on oct-31-2021 (MAS)

# function to filter detection based on overlap
filter_detection <- function(detection, by = "overlap", filter = "max", parallel = 1, pb = TRUE){

  if (is.null(detection$detection.class))
    stop("'detection.class' column not found in 'detection'. 'detection' must be the output of label_detection()")

  if (!by %in% names(detection))
    stop("'by' column not found")

  # add row id column to la
  detection$..row.id <- 1:nrow(detection)

  false.positives <- detection[grep("false.positive", detection$detection.class), ]
  true.positives <- detection[grep("true.positive", detection$detection.class, FALSE), ]

  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makeCluster(parallel) else
      cl <- parallel

  # run loop over every detected signal in the reference
  filter_tp_list <- warbleR:::pblapply_wrblr_int(X = unique(unlist(sapply(true.positives$reference.row, function(x) unlist(strsplit(x, "-")), USE.NAMES = FALSE))), cl = cl, pbar = pb, function(x){

    # get those detection that overlapped with x
    X <- true.positives[sapply(true.positives$reference.row, function(y) any(unlist(strsplit(y, "-")) == x)), ]

    # order by 'by'
    X <- X[order(X[, by, drop = TRUE], decreasing = TRUE), ]

    # filter
    if (filter == "max")
      X <- X[1, , drop = FALSE] else
        X <- X[nrow(X), , drop = FALSE]

    return(X)
  })

  # put together in a data frame
  filter_tp_df <- do.call(rbind, filter_tp_list)

  # add false positives
  detection <- rbind(false.positives, filter_tp_df)

  #sort back
  detection <- detection[order(detection$..row.id), ]

  # remove column with row names
  detection$..row.id <- NULL

  return(detection)
}
