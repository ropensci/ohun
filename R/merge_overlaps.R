#' @title Merge overlapping selections
#'
#' @description \code{merge_overlaps} merges several overlapping selections into a single selection
#' @usage merge_overlaps(X, pb = TRUE, cores = 1)
#' @param X Data frame or 'selection.table' (following the warbleR package format) with selections (start and end of the sound events). Must contained at least the following columns: "sound.files", "selec", "start" and "end".
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param cores Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @return If any time-overlapping selection is found it returns a data frame in which overlapping selections are collapse into a single selection.
#' @export
#' @name merge_overlaps
#' @details The function finds time-overlapping selection in reference tables and collapses them into a single selection. It can be useful to prepare reference tables to be used in an energy detection routine. In such cases overlapping selections are expected to be detected as a single sound. Therefore, merging them can be useful to prepare references in a format representing a more realistic expectation of how a perfect energy detection routine would look like.
#' @examples {
#'   # load data
#'   data("lbh_reference")
#'
#'   # nothing to merge
#'   merge_overlaps(lbh_reference)
#'
#'   # create artificial overlapping selections
#'   lbh_ref2 <- rbind(as.data.frame(lbh_reference[c(3, 10), ]), lbh_reference[c(3, 10), ])
#'
#'   lbh_ref2$selec <- seq_len(nrow(lbh_ref2))
#'
#'   merge_overlaps(lbh_ref2)
#' }
#' @seealso \code{\link{summarize_diagnostic}}, \code{\link{label_detection}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references 
#' Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C., Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. 2022. ohun: an R package for diagnosing and optimizing automatic sound event detection. BioRxiv, 2022.12.13.520253. https://doi.org/10.1101/2022.12.13.520253
#' 

merge_overlaps <- function(X, pb = TRUE, cores = 1) {
  # check arguments
  arguments <- as.list(base::match.call(expand.dots = FALSE))

  # do not check ... arguments
  arguments <- arguments[grep("...", names(arguments), fixed = TRUE, invert = TRUE)]

  # add objects to argument names
  for (i in names(arguments)[-1]) {
    arguments[[i]] <- get(i)
  }

  # check each arguments
  check_results <- check_arguments(fun = arguments[[1]], args = arguments)

  # report errors
  checkmate::reportAssertions(check_results)

  # merged overlapping selections
  ov_sls <-
    overlapping_sels(X,
      pb = pb,
      verbose = TRUE,
      parallel = cores
    )

  if (any(!is.na(ov_sls$ovlp.sels))) {
    merges_l <-
      warbleR:::.pblapply(unique(ov_sls$ovlp.sels), pbar = pb, message = "merging overlapping selections", current = 2, total = 2, cl = cores, function(x) {
        if (!is.na(x)) {
          Y <- ov_sls[ov_sls$ovlp.sels == x & !is.na(ov_sls$ovlp.sels), ]
          Y$end[1] <- max(Y$end)

          if (!is.null(Y$bottom.freq)) {
            Y$bottom.freq[1] <- min(Y$bottom.freq)
          }

          if (!is.null(Y$top.freq)) {
            Y$top.freq[1] <- max(Y$top.freq)
          }

          # return first row
          Y <- Y[1, ]
        } else {
          Y <- ov_sls[is.na(ov_sls$ovlp.sels), ]
        }
        return(Y)
      })

    ov_sls <- do.call(rbind, merges_l)

    ov_sls$ovlp.sels <- NULL
  } else {
    ov_sls <- X
  }

  # rename rows
  rownames(ov_sls) <- seq_len(nrow(ov_sls))

  return(ov_sls)
}
