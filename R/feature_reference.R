#' @title Extract quantitative features of references
#'
#' @description \code{feature_reference} extracts quantitative characteristics of a reference table
#' @usage feature_reference(reference, path = NULL, by.sound.file = FALSE)
#' @param reference Data frame or 'selection.table' (following the warbleR package format) with the reference selections (start and end of the signals) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end". If frequency range columns are included ("bottom.freq" and "top.freq") these are also used to characterize reference selections.
#' @param path Character string containing the directory path where the sound files are located. If supplied then duty cycle features are returned.
#' @param by.sound.file Logical argument to control whether features are summarized across sound files (when \code{by.sound.file = FALSE}, and more than 1 sound file is included in 'reference') or shown separated by sound file. Default is \code{FALSE}.
#' @return The function returns the mean, minimum and maximum duration of selections and gaps (time intervals between selections). If frequency range columns are included in the reference table (i.e. "bottom.freq" and "top.freq") the minimum bottom frequency ('min.bottom.freq') and the maximum top frequency ('max.top.freq') are also estimated. Finally, if the path to the sound files in 'reference' is supplied the duty cycle the mean, minimum and maximum. If `by.sound.file = FALSE` a matrix with features in rows is returned. Otherwise a data frame is returned in which each row correspond to a sound file.
#' @export
#' @name feature_reference
#' @details The function extract quantitative features from reference tables that can inform the range of values to be used in a energy-based detection optimization routine. Features related to selection duration can be used to set the 'max.duration' and 'min.duration' values, frequency related features can inform banpass values, gap related features inform hold time values and duty cycle can be used to evaluate performance. Note that duty cycle is not included as a diagnostic in other functions but it can also be estimated on detections using `feature_reference`.
#' @examples {
#' # load data
#' data("lbh_reference")
#'
#' # summary across sound files
#' feature_reference(reference = lbh_reference)
#'
#' # summary across sound files
#' feature_reference(reference = lbh_reference, by.sound.file = TRUE)
#' }
#' @seealso \code{\link{optimize_energy_detector}}, \code{\link{optimize_template_detector}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references {
#' Araya-Salas, M. (2021), ohun: automatic detection of acoustic signals. R package version 0.1.0.
#' }
# last modification on jan-2022 (MAS)

feature_reference <- function(reference, path = NULL, by.sound.file = FALSE) {

  internal_feature_reference <- function(reference, path = NULL){

    reference$duration <- reference$end - reference$start
    reference <- warbleR::gaps(X = reference, pb = FALSE)

    output <- data.frame(min.sel.duration = min(reference$duration, na.rm = TRUE))
    output$mean.sel.duration <- mean(reference$duration, na.rm = TRUE)
    output$max.sel.duration <- max(reference$duration, na.rm = TRUE)
    suppressWarnings(output$min.gap.duration <- min(reference$gaps, na.rm = TRUE))
    suppressWarnings(output$mean.gap.duration <- mean(reference$gaps, na.rm = TRUE))
    suppressWarnings(output$max.gap.duration <- max(reference$gaps, na.rm = TRUE))

    # frequency range descriptors
    if (!is.null(reference$bottom.freq) & !is.null(reference$top.freq)){
      output$min.bottom.freq <- min(reference$bottom.freq, na.rm = TRUE)
      output$max.top.freq <- max(reference$top.freq, na.rm = TRUE)
    }

    if (!is.null(path)){

      durs <- warbleR::duration_sound_files(files = unique(reference$sound.files), path = path)

      durs$duty.cycle <- sapply(1:nrow(durs), function(x)
        sum(reference$duration[reference$sound.files == durs$sound.files[x]], na.rm = TRUE) / durs$duration[x]
      )

      output$mean.duty.cycle <- mean(durs$duty.cycle, na.rm = TRUE)
      output$min.duty.cycle <- min(durs$duty.cycle, na.rm = TRUE)
      output$max.duty.cycle <- max(durs$duty.cycle, na.rm = TRUE)
    }

    return(output)
  }

  # force by.sound.files if only 1 sound file in reference
  if (length(unique(reference$sound.files)) == 1)
    by.sound.file <- TRUE

  if (!by.sound.file)
    output <- internal_feature_reference(reference, path) else {


      output_list <- lapply(unique(reference$sound.files), function(x){

        sub_output <- internal_feature_reference(reference = reference[reference$sound.files == x, ], path)
        sub_output$sound.files <- x
        return(sub_output)
      })


      output <- do.call(rbind, output_list)

      output$mean.gap.duration[is.infinite(output$mean.gap.duration) | is.nan(output$mean.gap.duration)] <- NA
      output$min.gap.duration[is.infinite(output$min.gap.duration) | is.nan(output$min.gap.duration)] <- NA

      # order columns
      output <- output[, c(ncol(output), 1:(ncol(output) - 1))]
    }

  # remove duty cycle range when by.sound.file = TRUE
  if (by.sound.file){
    output$min.duty.cycle <- output$max.duty.cycle <- NULL
  names(output)[names(output) == "mean.duty.cycle"] <- "duty.cycle"
  } else {
  # reformat as a matrix with mean min and max as columns
    other_feats <- c(output[, grep("\\.freq$", names(output), invert = TRUE)])

    if (!is.null(output$min.bottom.freq)){
      freq_feats <- c(output[, grep("\\.freq$", names(output))])
      freq_feats <- c(freq_feats[1], NA, NA, NA, NA, freq_feats[2])
      other_feats <- c(other_feats, freq_feats)
}
    output <- matrix(other_feats, ncol = 3, byrow = TRUE)
  colnames(output) <- c("min", "mean", "max")

  column_names <-  c("sel.duration", "gap.duration", "duty.cycle", "bottom.freq", "top.freq")

  if (is.null(path))
    column_names <- grep("duty.cycle", column_names, value = TRUE, invert = TRUE)

  rownames(output) <-column_names[1:nrow(output)]
}

  return(output)
}

