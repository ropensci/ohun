#' @title Extract quantitative features of references
#'
#' @description \code{feature_reference} extracts quantitative characteristics of a reference table
#' @usage feature_reference(reference, path = NULL, by.sound.file = FALSE,
#' units = c("ms", "kHz"), digits = 2)
#' @param reference Data frame or 'selection.table' (following the warbleR package format) with the reference selections (start and end of the sound events) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end". If frequency range columns are included ("bottom.freq" and "top.freq") these are also used to characterize reference selections.
#' @param path Character string containing the directory path where the sound files are located. If supplied then duty cycle and peak frequency features are returned. These features are more helpful for tuning a energy-based detection. Default is \code{NULL}.
#' @param by.sound.file Logical argument to control whether features are summarized across sound files (when \code{by.sound.file = FALSE}, and more than 1 sound file is included in 'reference') or shown separated by sound file. Default is \code{FALSE}.
#' @param units A character vector of length 2 with the units to be used for time and frequency parameters, in that order. Default is \code{c("ms", "kHz")}. It can also take 's' and 'Hz'.
#' @param digits Numeric vector of length 1 with the number of decimals to include. Default is 2.
#' @return The function returns the mean, minimum and maximum duration of selections and gaps (time intervals between selections) and of the number of annotations by sound file. If frequency range columns are included in the reference table (i.e. "bottom.freq" and "top.freq") the minimum bottom frequency ('min.bottom.freq') and the maximum top frequency ('max.top.freq') are also estimated. Finally, if the path to the sound files in 'reference' is supplied the duty cycle (fraction of a sound file corresponding to target sound events) and peak amplitude (highest amplitude in a detection) are also returned. If `by.sound.file = FALSE` a matrix with features in rows is returned. Otherwise a data frame is returned in which each row correspond to a sound file. By default, time features are returned in 'ms' while frequency features in 'kHz' (but see 'units' argument).
#' @export
#' @name feature_reference
#' @details The function extracts quantitative features from reference tables that can inform the range of values to be used in a energy-based detection optimization routine. Features related to selection duration can be used to set the 'max.duration' and 'min.duration' values, frequency related features can inform bandpass values, gap related features inform hold time values and duty cycle can be used to evaluate performance.
#' @examples {
#' # load data and save example files into temporary working directory
#' data("lbh1", "lbh2", "lbh_reference")
#' writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
#' writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
#'
#' # summary across sound files
#' feature_reference(reference = lbh_reference, path = tempdir())
#'
#' # summary across sound files
#' feature_reference(reference = lbh_reference, by.sound.file = TRUE, path = tempdir())
#' }
#' @seealso \code{\link{optimize_energy_detector}}, \code{\link{optimize_template_detector}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references {
#' Araya-Salas, M. (2021), ohun: diagnosing and optimizing automated sound event detection. R package version 0.1.0.
#' }
# last modification on jan-2022 (MAS)

feature_reference <-
  function(reference,
           path = NULL,
           by.sound.file = FALSE,
           units = c("ms", "kHz"),
           digits = 2) {
    if (is(reference, "extended_selection_table"))
      stop2(
        "The function is not defined for class 'extended_selection_table'. Use 'selection_table' or 'data.frame' instea."
      )

    internal_feature_reference <-
      function(reference,
               path = NULL,
               total.annotations = FALSE) {
        reference$duration <- reference$end - reference$start
        reference <- warbleR::gaps(X = reference, pb = FALSE)
        count_annotations <- table(reference$sound.files)

        output <-
          data.frame(min.sel.duration = min(reference$duration, na.rm = TRUE))
        output$mean.sel.duration <-
          mean(reference$duration, na.rm = TRUE)
        output$max.sel.duration <- max(reference$duration, na.rm = TRUE)
        suppressWarnings(output$min.gap.duration <-
                           min(reference$gaps, na.rm = TRUE))
        suppressWarnings(output$mean.gap.duration <-
                           mean(reference$gaps, na.rm = TRUE))
        suppressWarnings(output$max.gap.duration <-
                           max(reference$gaps, na.rm = TRUE))

        if (total.annotations)
          output$annotations <- nrow(reference) else
        {
          output$min.annotations <- min(count_annotations)
          output$mean.annotations <- mean(count_annotations)
          output$max.annotations <- max(count_annotations)
        }

        # frequency range descriptors
        if (!is.null(reference$bottom.freq) &
            !is.null(reference$top.freq)) {
          output$min.bottom.freq <- min(reference$bottom.freq, na.rm = TRUE)
          output$mean.bottom.freq <-
            mean(reference$bottom.freq, na.rm = TRUE)
          output$max.bottom.freq <-
            max(reference$bottom.freq, na.rm = TRUE)
          output$min.top.freq <- min(reference$top.freq, na.rm = TRUE)
          output$mean.top.freq <- mean(reference$top.freq, na.rm = TRUE)
          output$max.top.freq <- max(reference$top.freq, na.rm = TRUE)
        }

        if (!is.null(path)) {
          durs <-
            warbleR::duration_sound_files(files = unique(reference$sound.files),
                                          path = path)

          durs$duty.cycle <- sapply(1:nrow(durs), function(x)
            sum(reference$duration[reference$sound.files == durs$sound.files[x]], na.rm = TRUE) / durs$duration[x])

          output$min.duty.cycle <- min(durs$duty.cycle, na.rm = TRUE)
          output$mean.duty.cycle <- mean(durs$duty.cycle, na.rm = TRUE)
          output$max.duty.cycle <- max(durs$duty.cycle, na.rm = TRUE)

          # measure peak amplitude
          peak_amp <-
            warbleR::sound_pressure_level(reference,
                                          type = "peak",
                                          path = path,
                                          pb = FALSE)

          output$min.peak.amplitude <- min(peak_amp$SPL, na.rm = TRUE)
          output$mean.peak.amplitude <- mean(peak_amp$SPL, na.rm = TRUE)
          output$max.peak.amplitude <- max(peak_amp$SPL, na.rm = TRUE)
        }

        return(output)
      }

    # force by.sound.files if only 1 sound file in reference
    if (length(unique(reference$sound.files)) == 1)
      by.sound.file <- TRUE

    if (!by.sound.file)
      output <-
      internal_feature_reference(reference, path, total.annotations = FALSE) else {
      output_list <- lapply(unique(reference$sound.files), function(x) {
        sub_output <-
          internal_feature_reference(reference = reference[reference$sound.files == x,],
                                     path,
                                     total.annotations = TRUE)
        sub_output$sound.files <- x
        return(sub_output)
      })

      output <- do.call(rbind, output_list)

      output$mean.gap.duration[is.infinite(output$mean.gap.duration) |
                                 is.nan(output$mean.gap.duration)] <- NA
      output$min.gap.duration[is.infinite(output$min.gap.duration) |
                                is.nan(output$min.gap.duration)] <- NA

      # order columns
      output <- output[, c(ncol(output), 1:(ncol(output) - 1))]
    }

    # remove duty cycle range when by.sound.file = TRUE
    if (by.sound.file) {
      output$min.duty.cycle <- output$max.duty.cycle <- NULL
      names(output)[names(output) == "mean.duty.cycle"] <- "duty.cycle"
    } else {
      # reformat as a matrix with mean min and max as columns
      other_feats <-
        c(output[, grep("\\.freq$", names(output), invert = TRUE)])

      # order columns
      if (!is.null(output$min.bottom.freq)) {
        freq_feats <- c(output[, grep("\\.freq$", names(output))])
        other_feats <- c(other_feats, freq_feats)
      }
      output <- matrix(unlist(other_feats), ncol = 3, byrow = TRUE)
      colnames(output) <- c("min", "mean", "max")

      row_names <-
        c(
          "sel.duration",
          "gap.duration",
          "annotations",
          "duty.cycle",
          "peak.amplitude",
          "bottom.freq",
          "top.freq"
        )

      if (is.null(path))
        row_names <-
        grep("duty.cycle|peak.amplitude",
             row_names,
             value = TRUE,
             invert = TRUE)

      rownames(output) <- row_names[1:nrow(output)]
    }

    # round digits and change units
    if (is.matrix(output)) {
      # fix units
      if (units[1] == "ms")
        for (i in grep("duration$", rownames(output)))
          output[i,] <- output[i,] * 1000

      if (units[2] == "Hz")
        for (i in grep("freq$", rownames(output)))
          output[i,] <- output[i,] * 1000

      # round
      output <- round(x = output, digits = digits)

    } else {
      # fix units
      if (units[1] == "ms")
        for (i in grep("duration$", colnames(output)))
          output[, i] <- output[, i] * 1000

      if (units[2] == "Hz")
        for (i in grep("freq$", colnames(output)))
          output[, i] <- output[, i] * 1000

      for (i in 2:ncol(output))
        output[, i] <- round(x = output[, i], digits = digits)

    }
    return(output)
  }
