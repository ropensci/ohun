#' @title Plot detection and reference annotations
#'
#' @description \code{plot_detection} evaluates the performance of a sound event detection procedure comparing the output selection table to a reference selection table
#' @usage plot_detection(reference, detection, mid.point = FALSE, size = 20, positions = c(1, 2))
#' @param reference Data frame or 'selection.table' (following the warbleR package format) with the reference selections (start and end of the sound events) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end". \strong{It must contain the reference selections that will be used for detection optimization}.
#' @param detection Data frame or 'selection.table' with the detections (start and end of the sound events) that will be compared against the 'reference' selections. Must contained at least the following columns: "sound.files", "selec", "start" and "end". It can contain data for additional sound files not found in 'references'. In this case the routine assumes that no sound events are found in those files, so detection from those files are all false positives.
#' @param mid.point Logical argument to control if each annotations is shown as a rectangle with fix width center at the mid point of the time position (if \code{TRUE}) or the true time range of the annotations is used (if \code{FALSE},  default). 'mid.point' can be useful to make visible annotations in very long sound files that would otherwise look to thin.
#' @param size Numeric. Controls the size of the rectangles if \code{mid.point = TRUE}. Default is 20.
#' @param positions Numeric. Controls the vertical position of the rectangles representing anotations. Default is c(1, 2). This can be used to get reference and detection annotations closer in the vertical axis. Note that the height of rectangles is 0.5.
#' @return A ggplot graph (i.e. an object of class "ggplot").
#' @export
#' @name plot_detection
#' @details The function helps to visualize the match between reference and detection annotations by plotting them next to each other as rectangles along the time axis. If the annotations contain data for several sound files each sound file will be plotted in its own panel. The plot can be further modify by users using regular ggplot syntax.
#' @examples {
#'   # load data
#'   data("lbh_reference")
#'
#'   # mid point and regular size
#'   plot_detection(
#'     reference = lbh_reference[-14, ],
#'     detection = lbh_reference[-1, ], mid.point = TRUE
#'   )
#'
#'   # mid point and larger size
#'   plot_detection(
#'     reference = lbh_reference[-14, ],
#'     detection = lbh_reference[-1, ], mid.point = TRUE, size = 25
#'   )
#'
#'   # true time rectangles
#'   plot_detection(
#'     reference = lbh_reference[-14, ],
#'     detection = lbh_reference[-1, ]
#'   )
#'
#'   # use position to make reference and anotations overlap vertically
#'   plot_detection(
#'     reference = lbh_reference[-14, ],
#'     detection = lbh_reference[-1, ], positions = c(1, 1.4)
#'   )
#'
#'   # modified using ggplot
#'   gg_pd <- plot_detection(
#'     reference = lbh_reference[-14, ],
#'     detection = lbh_reference[-1, ], positions = c(1, 1.4)
#'   )
#'
#'   gg_pd + ggplot2::theme_classic(base_size = 25)
#' }
#' @seealso \code{\link{label_spectro}}, \code{\link{diagnose_detection}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references {
#' Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C., Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. 2022. ohun: an R package for diagnosing and optimizing automatic sound event detection. BioRxiv, 2022.12.13.520253. https://doi.org/10.1101/2022.12.13.520253
#' }

plot_detection <-
  function(reference,
           detection,
           mid.point = FALSE,
           size = 20,
           positions = c(1, 2)) {
    # check arguments
    arguments <- as.list(base::match.call())

    # add objects to argument names
    for (i in names(arguments)[-1]) {
      arguments[[i]] <- get(i)
    }

    # check each arguments
    check_results <-
      check_arguments(fun = arguments[[1]], args = arguments)

    # report errors
    checkmate::reportAssertions(check_results)

    # set empty objects to avoid conflict with object names in ggplot2 functions
    .type <- x <- y <- id <- NULL

    # add column to ID observations from each input data set
    reference$.type <- "reference"
    detection$.type <- "detection"

    # bind data sets
    common_cols <- intersect(names(reference), names(detection))
    dat <-
      rbind(as.data.frame(reference[, common_cols]), as.data.frame(detection[, common_cols]))

    # make type a factor to order position of reference and detection
    dat$.type <-
      factor(dat$.type, levels = c("detection", "reference"))

    if (mid.point) {
      # add mid time
      dat$time <- dat$start + ((dat$end - dat$start) / 2)

      # generate ggplot object
      gg_obj <-
        ggplot(data = dat, aes(x = time, y = .type, color = .type)) +
        geom_point(
          size = size,
          shape = 124,
          show.legend = FALSE
        ) +
        scale_color_manual(values = c("#48287880", "#35B77980")) +
        facet_grid(sound.files ~ .) +
        labs(x = "Time (s)", y = "") +
        theme_bw(base_size = 12)
    } else {
      # create a unique ID for each row
      dat$id <- paste(dat$sound.files, dat$selec, dat$.type, sep = "-")

      corner_positions <-
        c(-0.25, -0.25, 0.25, 0.25) + rep(positions, each = 4)

      # add x and y coordinates for each corner of the polygon representing the sound
      xy_dat_list <- lapply(unique(dat$id), function(x) {
        X <- dat[dat$id == x, ]
        X2 <- X[rep(1, 4), ]
        X2$x <- c(X$start, X$end, X$end, X$start)
        X2$y <- if (X$.type == "reference") {
          corner_positions[1:4]
        } else {
          corner_positions[5:8]
        }
        return(X2)
      })

      # combine in a single data frame
      xy_dat <- do.call(rbind, xy_dat_list)

      # generate ggplot object
      gg_obj <- ggplot(xy_dat, aes(x = x, y = .type, fill = .type)) +
        geom_polygon(aes(x = x, y = y, group = id), show.legend = FALSE) +
        facet_grid(sound.files ~ .) +
        labs(x = "Time (s)", y = "") +
        scale_fill_manual(values = c("#48287880", "#35B77980")) +
        scale_y_continuous(
          breaks = positions,
          labels = c("Reference", "Detection")
        ) +
        theme_bw(base_size = 12)
    }

    return(gg_obj)
  }
