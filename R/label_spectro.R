#' @title Plot a labeled spectrogram
#'
#' @description \code{label_spectro} plot a spectrogram along with amplitude envelopes or cross-correlation scores
#' @usage label_spectro(wave, reference = NULL, detection = NULL,
#'  envelope = FALSE, threshold = NULL, smooth = 5, collevels = seq(-100, 0, 5),
#'  palette = viridis::viridis, template.correlation = NULL,
#'  line.x.position = 2, hop.size = NULL, ...)
#' @param wave A 'wave' class object.
#' @param detection Data frame or selection table (using the warbleR package's format, see \code{\link[warbleR]{selection_table}}).
#' @param reference Data frame or 'selection.table' (following the warbleR package format) with the reference selections (start and end of the sound events). Must contained at least the following columns: "sound.files", "selec", "start" and "end".
#' @param detection Data frame or 'selection.table' with the detection (start and end of the sound events) Must contained at least the following columns: "sound.files", "selec", "start" and "end".
#' @param envelope Logical to control whether the amplitude envelope is plotted. Default is \code{FALSE}.
#' @param threshold A numeric vector on length 1 indicated the amplitude or correlation threshold to plot on the envelope or correlation scores respectively. Default is \code{NULL}. Note that for amplitude the range of valid values is 0-1, while for correlations the range is 0-100.
#' @param smooth A numeric vector of length 1 to smooth the amplitude envelope
#'   with a sum smooth function. It controls the time range (in ms) in which amplitude samples are smoothed (i.e. averaged with neighboring samples). Default is 5. 0 means no smoothing is applied.
#' @param collevels Numeric sequence of negative numbers to control color partitioning and amplitude values that are shown (as in \code{\link[seewave]{spectro}}).
#' @param palette Function with the color palette to be used on the spectrogram (as in \code{\link[seewave]{spectro}})
#' @param template.correlation List extracted from the output of \code{\link{template_correlator}} containing the correlation scores and metadata for an specific sound file/template dyad. For instance 'correlations[[1]]' where 'correlations' is the output of a \code{\link{template_correlator}} call. If supplied the correlation is also plotted. Default is \code{NULL}.
#' @param line.x.position Numeric vector of length 1 with the position in the frequency axis (so in kHz) of the lines highlighting sound events. Default is 2.
#' @param hop.size A numeric vector of length 1 specifying the time window duration (in ms). Default is 11.6 ms, which is equivalent to 512 'wl' for a 44.1 kHz sampling rate.
#' @param ... Additional arguments to be passed to  \code{\link[seewave]{spectro}} for further spectrogram customization.
#' @return A spectrogram along with lines highlighting the position of sound events in 'reference' and/or 'detection'. If supplied it will also plot the amplitude envelope or corelation scores below the spectrogram.
#' @export
#' @name label_spectro
#' @details This function plots spectrograms annotated with the position of sound events. \strong{Created for graphs included in the vignette, and probably only useful for that or for very short recordings}. Only works on a single 'wave' object at the time.
#'
#' @examples {
#'   # load example data
#'   data(list = "lbh1", "lbh_reference")
#'
#'   # adding labels
#'   label_spectro(
#'     wave = lbh1,
#'     reference = lbh_reference[lbh_reference$sound.files == "lbh1.wav", ],
#'     wl = 200, ovlp = 50, flim = c(1, 10)
#'   )
#'
#'   # adding envelope
#'   label_spectro(
#'     wave = lbh1,
#'     detection = lbh_reference[lbh_reference$sound.files == "lbh1.wav", ],
#'     wl = 200, ovlp = 50, flim = c(1, 10)
#'   )
#'
#'   # see the package vignette for more examples
#' }
#'
#' @references
#' #' Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C., Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. 2022. ohun: an R package for diagnosing and optimizing automatic sound event detection. BioRxiv, 2022.12.13.520253. https://doi.org/10.1101/2022.12.13.520253
#'
#' @seealso \code{\link{energy_detector}}, \code{\link{template_correlator}}, \code{\link{template_detector}}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}).

label_spectro <-
  function(wave,
           reference = NULL,
           detection = NULL,
           envelope = FALSE,
           threshold = NULL,
           smooth = 5,
           collevels = seq(-100, 0, 5),
           palette = viridis::viridis,
           template.correlation = NULL,
           line.x.position = 2,
           hop.size = NULL,
           ...) {
    # error message if wavethresh is not installed
    if (!requireNamespace("viridis", quietly = TRUE)) {
      stop2("must install 'viridis' to use this function")
    }

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

    # adjust wl based on hope.size
    if (!is.null(hop.size)) {
      wl <- round(wave@samp.rate * hop.size / 1000, 0)
    }

    # reset graphic device on exit
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))

    if (envelope | !is.null(template.correlation)) {
      par(mfrow = c(2, 1), mar = c(0, 4, 1, 1))
    } else {
      par(mar = c(4, 4, 1, 1))
    }

    # plot spectrogram
    seewave::spectro(
      wave = wave,
      grid = FALSE,
      scale = FALSE,
      palette = palette,
      collevels = collevels,
      axisX = if (envelope |
        !is.null(template.correlation)) {
        FALSE
      } else {
        TRUE
      },
      ...
    )

    # plot detection
    if (!is.null(reference)) {
      for (i in seq_len(nrow(reference))) {
        lines(
          x = (reference[i, c("start", "end")]),
          y = rep(line.x.position, 2),
          col = "#F7D03CFF",
          lwd = 7,
          lend = 2
        )
      }
    }

    # plot detection
    if (!is.null(detection)) {
      for (i in seq_len(nrow(detection))) {
        lines(
          x = (detection[i, c("start", "end")]),
          y = rep(line.x.position - 0.3, 2),
          col = "#CF4446FF",
          lwd = 7,
          lend = 2
        )
      }
    }

    usr <- par("usr")

    # add legend
    if (!is.null(detection) & !is.null(reference)) {
      legend(
        x = usr[2] * 0.98,
        y = usr[4] * 0.98,
        col = c("#F7D03CFF", "#CF4446FF"),
        legend = c("reference", "detection"),
        lwd = 4,
        bg = "#FFFFFFE6",
        xjust = 1,
        yjust = 1
      )
    }

    if (is.null(detection) & !is.null(reference)) {
      legend(
        x = usr[2] * 0.98,
        y = usr[4] * 0.98,
        col = c("#F7D03CFF"),
        legend = c("reference"),
        lwd = 4,
        bg = "#FFFFFFE6",
        xjust = 1,
        yjust = 1
      )
    }

    if (is.null(reference) & !is.null(detection)) {
      legend(
        x = usr[2] * 0.98,
        y = usr[4] * 0.98,
        col = c("#CF4446FF"),
        legend = c("detection"),
        lwd = 4,
        bg = "#FFFFFFE6",
        xjust = 1,
        yjust = 1
      )
    }

    if (envelope) {
      # set graphic device for envelope
      par(mar = c(4, 4, 0.3, 1))

      if (!is.null(smooth)) {
        smooth <- round(wave@samp.rate * smooth / 1000, 0)
      }

      # plot envelope
      env_obj <- seewave::env(wave, colwave = "#07889B", ssmooth = smooth, plot = FALSE, norm = TRUE)
      plot(y = env_obj[, 1], x = seq(0, seewave::duration(wave), length.out = nrow(env_obj)), type = "l", col = "#07889B", xlab = "Time", ylab = "Amplitude", yaxt = "n", xaxs = "i", yaxs = "i", ylim = c(0, 1.1))

      # add threshold line
      if (!is.null(threshold)) {
        abline(
          h = par("usr")[4] * threshold / 100,
          col = "#CF4446FF",
          lwd = 3
        )
      }
    } else if (!is.null(template.correlation)) {
      # set graphic device for correlations
      par(mar = c(4, 4, 0.3, 1))

      plot(
        x = seq(
          template.correlation$template.duration / 2,
          duration(wave) - template.correlation$template.duration / 2,
          length.out = length(template.correlation$correlation.scores)
        ),
        y = template.correlation$correlation.scores,
        type = "l",
        xlab = "Time (s)",
        ylab = "Correlation",
        col = "#07889B",
        lwd = 1.6,
        xaxs = "i",
        xlim = c(0, duration(wave))
      )

      # add threshold line
      if (!is.null(threshold)) {
        abline(h = threshold, col = "#CF4446FF", lwd = 3)
      }
    }
  }
