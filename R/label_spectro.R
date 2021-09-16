#' @title Plot a labeled spectrogram
#'
#' @description \code{label_spectro} plot a spectrogram along with amplitude envelopes or cross-corelation scores
#' @usage label_spectro(wave, reference = NULL, detection = NULL,
#'  envelope = FALSE, threshold = NULL, smooth = 5, collevels = seq(-100, 0, 5),
#'  palette = viridis::viridis, template.correlation = NULL,
#'  line.x.position = 2, ...)
#' @param wave A 'wave' class object.
#' @param detection Data frame or selection table (using the warbleR package's format, see \code{\link[warbleR]{selection_table}}).
#' @param reference Data frame or 'selection.table' (following the warbleR package format) with the reference selections (start and end of the signals). Must contained at least the following columns: "sound.files", "selec", "start" and "end".
#' @param detection Data frame or 'selection.table' with the detection (start and end of the signals) Must contained at least the following columns: "sound.files", "selec", "start" and "end".
#' @param envelope Logical to control whether the amplitude envelope is plotted. Default is \code{FALSE}.
#' @param threshold A numeric vector on length 1 indicated the amplitude or correlation threshold to plot on the envelope or correlation scores respectively. Default is \code{NULL}.
#' @param smooth A numeric vector of length 1 to smooth the amplitude envelope
#'   with a sum smooth function. It controls the time range (in ms) in which amplitude samples are smoothed (i.e. averaged with neighboring samples). Default is 5. 0 means no smoothing is applied.
#' @param collevels Numeric sequence of negative numbers to control color partitioning and amplitude values that are shown (as in \code{\link[seewave]{spectro}}).
#' @param palette Function with the color palette to be used on the spectrogram (as in \code{\link[seewave]{spectro}})
#' @param template.correlation Numeric vector with the cross-correlation scores from \code{\link{template_correlator}}. If supplied the correlation is also plotted. Default is \code{NULL}.
#' @param line.x.position Numeric vector of length 1 with the position in the frequency axis (so in kHz) of the lines highlighting signals. Default is 2.
#' @param ... Additional arguments to be passed to  \code{\link[seewave]{spectro}} for further spectrogram customization.
#' @return A spectrogram along with lines highlighting the position of signals in 'reference' and/or 'detection'. If supplied it will also plot the amplitude envelope or corelation scores below the spectroram.
#' @export
#' @name label_spectro
#' @details This function plots spectrograms annotated with the position of signals. Mostly created for graphs included in the vignette. Only works on a single 'wave' object at the time.
#'
#' @examples {
#' # load example data
#' data(list = "Phae.long1")
#'
#'# adding labels
#' label_spectro(wave = Phae.long1,
#' reference = lbh_selec_reference[lbh_selec_reference$sound.files == "Phae.long1.wav", ],
#' wl = 200, ovlp = 50, flim = c(1, 10))
#'
#' # adding envelope
#' label_spectro(wave = Phae.long1,
#' detection = lbh_selec_reference[lbh_selec_reference$sound.files == "Phae.long1.wav", ],
#' wl = 200, ovlp = 50, flim = c(1, 10))
#'
#' # see the package vignette for more examples
#' }
#'
#' @references {
#'#' Araya-Salas, M. (2021), ohun: automatic detection of acoustic signals. R package version 0.1.0.
#' }
#' @seealso \code{\link{label_detection}}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}).
#last modification on oct-31-2021 (MAS)
label_spectro <- function(wave, reference = NULL, detection = NULL, envelope = FALSE, threshold = NULL, smooth = 5, collevels = seq(-100, 0, 5), palette = viridis::viridis, template.correlation = NULL, line.x.position = 2, ...) {

  # set graphic device
  on.exit(suppressWarnings(par(mfrow = c(1, 1), mar=c(5, 4, 4, 2) +0.1)))

  if (envelope | !is.null(template.correlation))
    par(mfrow = c(2, 1), mar = c(0,  4,  1,  1)) else
      par(mar = c(4, 4, 1, 1))

  # plot spectrogram
  seewave::spectro(wave = wave, grid = FALSE, scale = FALSE, palette = palette, collevels = collevels, axisX = if (envelope | !is.null(template.correlation)) FALSE else TRUE,...)

  # plot detection
  if (!is.null(reference))
    for(i in 1:nrow(reference))
      lines(x = (reference[i, c("start", "end")]), y = rep(line.x.position, 2), col = "#F7D03CFF", lwd = 7, lend = 2)

  # plot detection
  if (!is.null(detection))
    for(i in 1:nrow(detection))
      lines(x = (detection[i, c("start", "end")]), y = rep(line.x.position - 0.3, 2), col = "#CF4446FF", lwd = 7, lend = 2)

  usr <- par("usr")

  # add legend
  if (!is.null(detection) & !is.null(reference))
    legend(x = usr[2] * 0.98, y = usr[4] * 0.98, col = c("#F7D03CFF", "#CF4446FF"), legend = c("reference", "detection"), lwd = 4, bg = "#FFFFFFE6", xjust = 1, yjust = 1)

  if (is.null(detection) & !is.null(reference))
    legend(x = usr[2] * 0.98, y = usr[4] * 0.98, col = c("#F7D03CFF"), legend = c("reference"), lwd = 4, bg = "#FFFFFFE6",  xjust = 1, yjust = 1)

  if (is.null(reference) & !is.null(detection))
    legend(x = usr[2] * 0.98, y = usr[4] * 0.98, col = c("#CF4446FF"), legend = c("detection"), lwd = 4, bg = "#FFFFFFE6",  xjust = 1, yjust = 1)

  if (envelope) {
    # set graphic device for envelope
    par(mar = c(4,  4,  0.3,  1))

    if (!is.null(smooth))
      smooth <- round(wave@samp.rate * smooth  / 1000, 0)

    # plot envelope
    seewave::env(wave, colwave = "#07889B", ssmooth = smooth)

    # add threshold line
    if (!is.null(threshold))
      abline(h = par("usr")[4] * threshold, col = "#CF4446FF", lwd = 3)
  } else
    if (!is.null(template.correlation)) {
      # set graphic device for correlations
      par(mar = c(4,  4,  0.3,  1))

      plot(x = seq(0, duration(wave), length.out = length(template.correlation)), y = template.correlation, type = "l", xlab = "Time (s)", ylab = "Correlation", col = "#07889B", lwd = 1.6, xaxs = "i")

      # add threshold line
      if (!is.null(threshold))
        abline(h = threshold, col = "#CF4446FF", lwd = 3)

    }
}
