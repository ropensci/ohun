# internal function not to be called by users
# stop function that doesn't print call
stop2 <- function(...) {
  stop(..., call. = FALSE)
}

# warning function that doesn't print call
warning2 <- function(...) {
  warning(..., call. = FALSE)
}


# message function that changes colors
message2 <- function(x, color = "black") {
  message(colortext(x, as = color))
}


colortext <-
  function(text,
           as = c(
             "red",
             "blue",
             "green",
             "magenta",
             "cyan",
             "orange",
             "black",
             "silver"
           )) {
    if (has_color()) {
      unclass(cli::make_ansi_style(ohun_style(as))(text))
    } else {
      text
    }
  }

# to check if string includes color
has_color <- function() {
  cli::num_ansi_colors() > 1
}

# to print text in colors
ohun_style <-
  function(color = c(
             "red",
             "blue",
             "green",
             "magenta",
             "cyan",
             "orange",
             "black",
             "silver"
           )) {
    type <- match.arg(color)

    c(
      red = "red",
      blue = "blue",
      green = "green",
      magenta = "magenta",
      cyan = "cyan",
      orange = "orange",
      black = "black",
      silver = "silver"
    )[[color]]
  }



# internal function to find (and plot) templates in the acoustic space used by get_templates()
find_templates <-
  function(reference = NULL,
           n.sub.spaces,
           space = NULL,
           plot = TRUE,
           color = "#21908C4D",
           xlab = "Dimension 1",
           ylab = "Dimension 2") {
    x <- rep(1, n.sub.spaces)
    space <- space[, 1:2]
    space <- data.frame(space)
    space$...NROW <- seq_len(nrow(space))

    # mean center
    mean_dim1 <- mean(space[, 1])
    mean_dim2 <- mean(space[, 2])

    # center at 0
    space[, 1] <- space[, 1] - mean_dim1
    space[, 2] <- space[, 2] - mean_dim2

    x <- c(0, cumsum(x) / sum(x))
    dx <- diff(x)
    nx <- length(dx)
    radius <- 100

    t2xy <- function(t) {
      t2p <- 2 * pi * t + 0 * pi / 180
      list(x = radius * cos(t2p), y = radius * sin(t2p))
    }

    if (plot) {
      plot(
        space[, 1] + mean_dim1,
        space[, 2] + mean_dim2,
        pch = 20,
        cex = 2,
        col = color,
        xlab = xlab,
        ylab = ylab
      )
    }

    # get polygon for each sub space
    if (n.sub.spaces > 1) {
      polys <- lapply(1:nx, function(i) {
        n <- max(2, floor(100 * dx[i])) # 100 equivalent to edges in pie()
        P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))

        if (plot) {
          lines(
            c(P$x[1], 0) + mean_dim1,
            c(P$y[1], 0) + mean_dim2,
            lty = 3,
            col = "gray",
            lwd = 3
          )
        }

        pol_df <- data.frame(x = c(P$x, 0), y = c(P$y, 0))
        pol_df_closed <- rbind(pol_df[nrow(pol_df), ], pol_df)
        poly <- sf::st_polygon(list(as.matrix(pol_df_closed)))

        return(poly)
      })
    } else {
      polys <- vector()
    }

    # get  centroids, the first one is the centroid of the enitre space
    centroids <- vector(length = length(polys) + 1)

    for (i in 0:length(polys)) {
      if (i > 0) {
        whch_within <-
          which(sf::st_intersects(sf::st_as_sf(
            x = data.frame(x = space[, 1], y = space[, 2]),
            coords = c("x", "y")
          ), polys[[i]], sparse = FALSE) == 1)
      } else {
        whch_within <- seq_len(nrow(space))
      }

      if (length(whch_within) > 0) {
        if (length(whch_within) > 1) {
          sub_space <- space[whch_within, ]

          # get centroid
          centroid_coors <- colMeans(sub_space[, 1:2])

          # and distance to centroid
          dists_to_centroid <-
            unlist(lapply(seq_len(nrow(sub_space)), function(x) {
              stats::dist(
                rbind(sub_space[x, 1:2], centroid_coors)
              )
            }))

          centroid <-
            sub_space$...NROW[which.min(dists_to_centroid)]
        } else {
          centroid <- sub_space$...NROW[whch_within]
        }
      } else {
        centroid <- NA
      }

      centroids[i + 1] <- centroid
    }


    if (length(centroids) > 1) {
      if (any(centroids[-1] == centroids[1])) {
        message2(
          color = "silver",
          x = paste(
            sum(centroids[-1] == centroids[1]),
            "sub-space centroid(s) coincide with the overall centroid and was (were) removed"
          )
        )
        centroids <-
          c(centroids[1], centroids[-1][centroids[-1] != centroids[1]])
      }
    }

    names(centroids) <-
      if (length(centroids) > 1) {
        c("centroid", paste0("templ-", 1:(length(centroids) - 1)))
      } else {
        "centroid"
      }




    # plot
    if (plot) {
      points(
        x = space[centroids, 1] + mean_dim1,
        y = space[centroids, 2] + mean_dim2,
        col = "#FDE725FF",
        pch = 1,
        cex = 1.8,
        lwd = 4
      )

      text(
        x = space[centroids, 1] + mean_dim1,
        y = space[centroids, 2] + mean_dim2,
        labels = names(centroids),
        pos = 4
      )
    }

    return(centroids)
  }

# find which detections overlap with which references
overlapping_detections <- function(reference, detection) {
  overlapping_pairs <- lapply(1:nrow(detection), function(i) {
    start1 <- detection[i, "start"]
    end1 <- detection[i, "end"]

    indices <- which((end1 > reference$start) & (start1 < reference$end))

    if (length(indices) > 0) {
      overlapping_df <- data.frame(sound.files = detection$sound.files[i], detection.id = rep(paste(detection$sound.files[i], detection$selec[i], sep = "-"), length(indices)), reference.id = vapply(indices, function(j) paste(reference$sound.files[j], reference$selec[j], sep = "-"), FUN.VALUE = character(1)))

      return(overlapping_df)
    } else {
      return(NULL)
    }
  })

  overlapping_df <- do.call(rbind, Filter(NROW, overlapping_pairs))

  return(overlapping_df)
}

# calculate intersection by union overlap for a single pair
calculate_iou <- function(reference, detection) {
  intersection <- max(0, min(detection$end, reference$end) - max(detection$start, reference$start))
  union <- max(0, max(detection$end, reference$end) - min(detection$start, reference$start))

  iou <- intersection / union

  return(iou)
}

# calculate intersection by union overlap for several pairs
pairs_iou <- function(df, detection, reference) {
  if (!is.null(df)) {
    detection$id <- paste(detection$sound.files, detection$selec, sep = "-")
    reference$id <- paste(reference$sound.files, reference$selec, sep = "-")

    iou_values <- sapply(1:nrow(df), function(i) {
      detection.id <- df$detection.id[i]
      reference.id <- df$reference.id[i]

      detection_event <- detection[detection$id == detection.id, ]
      ground_truth_event <- reference[reference$id == reference.id, ]

      iou <- calculate_iou(detection_event, ground_truth_event)
      return(iou)
    })

    df$IoU <- iou_values
  } else {
    df <- data.frame(sound.files = vector(), detection.id = vector(), reference.id = vector(), IoU = vector())
  }

  return(df)
}

.onAttach <-
  function(libname, pkgname) {
    packageStartupMessage("\nPlease cite 'ohun' as: \n")
    packageStartupMessage(" Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C., Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. 2023. ohun: an R package for diagnosing and optimizing automatic sound event detection. Methods in Ecology and Evolution. https://doi.org/10.1111/2041-210X.14170")
  }


# custom assert functions to check arguments
# Simple custom check function  no duplicated selection labels
check_unique_sels <- function(x, fun) {
  if (fun == "label_detection") {
    if (anyDuplicated(paste(x$sound.files, x$selec, x$template)) > 0) "Duplicated 'selec' labels within at least one combination of sound file/template" else TRUE
  } else {
    if (anyDuplicated(paste(x$sound.files, x$selec)) > 0) "Duplicated 'selec' labels within at least one sound file" else TRUE
  }
}

assert_unique_sels <- checkmate::makeAssertionFunction(check_unique_sels)

## function to check arguments
check_arguments <- function(fun, args) {
  # create object to store check results
  check_collection <- checkmate::makeAssertCollection()

  ### check arguments
  if (any(names(args) == "reference")) {
    checkmate::assert_multi_class(x = args$reference, classes = c("data.frame", "selection.table"), add = check_collection, .var.name = "reference")
    checkmate::assert_names(x = names(args$reference), type = "unique", must.include = c("sound.files", "selec", "start", "end"), add = check_collection, .var.name = "names(reference)")
    try(checkmate::assert_data_frame(x = args$reference[, c("sound.files", "selec", "start", "end")], any.missing = FALSE, add = check_collection, .var.name = "reference"), silent = TRUE)
    assert_unique_sels(x = args$reference, fun = fun, add = check_collection, .var.name = "reference")
  }

  if (any(names(args) == "detection")) {
    checkmate::assert_data_frame(x = args$detection, any.missing = TRUE, add = check_collection, .var.name = "detection")
    checkmate::assert_multi_class(x = args$detection, classes = c("data.frame", "selection.table"), add = check_collection, .var.name = "detection")

    cols <- if (fun == "consensus_detection") {
      c("sound.files", "selec", "start", "end", "detection.class")
    } else {
      c("sound.files", "selec", "start", "end")
    }

    checkmate::assert_names(x = names(args$detection), type = "unique", must.include = cols, add = check_collection, .var.name = "names(detection)")
    try(checkmate::assert_data_frame(x = args$detection[, cols], any.missing = TRUE, add = check_collection, .var.name = "detection"), silent = TRUE)

    assert_unique_sels(x = args$detection, fun = fun, add = check_collection, .var.name = "detection")
  }

  if (any(names(args) == "X")) {
    checkmate::assert_multi_class(x = args$X, classes = c("data.frame", "selection.table"), add = check_collection, .var.name = "X")
    checkmate::assert_names(x = names(args$X), type = "unique", must.include = c("sound.files", "selec", "start", "end"), add = check_collection, .var.name = "names(X)")
    try(checkmate::assert_data_frame(x = args$X[, c("sound.files", "selec", "start", "end")], any.missing = FALSE, add = check_collection, .var.name = "X"), silent = TRUE)
  }

  if (any(names(args) == "templates")) {
    checkmate::assert_multi_class(x = args$templates, classes = c("data.frame", "selection.table", "extended.selection.table"), add = check_collection, .var.name = "templates")
    checkmate::assert_names(x = names(args$templates), type = "unique", must.include = c("sound.files", "selec", "start", "end"), add = check_collection, .var.name = "names(templates)")
    try(checkmate::assert_data_frame(x = args$templates[, c("sound.files", "selec", "start", "end")], any.missing = FALSE, add = check_collection, .var.name = "templates"), silent = TRUE)
    assert_unique_sels(x = args$templates, fun = fun, add = check_collection, .var.name = "templates")
  }

  if (any(names(args) == "previous.output")) {
    checkmate::assert_data_frame(x = args$previous.output, any.missing = TRUE, add = check_collection, .var.name = "previous.output")
  }

  if (any(names(args) == "diagnostic")) {
    checkmate::assert_data_frame(x = args$diagnostic, any.missing = TRUE, add = check_collection, .var.name = "diagnostic")
  }

  # if (any(names(args) == "files"))
  #   if (!is.null(args$files))
  #   checkmate::assert_file_exists(x = args$files, access = "r", extension = c("mp3", "wav", "wac", "flac"), add = check_collection, .var.name = "files")

  if (any(names(args) == "by.sound.file")) {
    checkmate::assert_logical(x = args$by.sound.file, len = 1, add = check_collection, .var.name = "by.sound.file")
  }

  if (any(names(args) == "time.diagnostics")) {
    checkmate::assert_logical(x = args$time.diagnostics, len = 1, add = check_collection, .var.name = "time.diagnostics")
  }

  if (any(names(args) == "cores")) {
    checkmate::assert_integerish(args$cores, add = check_collection, lower = 1, upper = parallel::detectCores(), .var.name = "cores")
  }

  if (any(names(args) == "pb")) {
    checkmate::assert_logical(x = args$pb, len = 1, add = check_collection, .var.name = "pb")
  }

  if (any(names(args) == "path")) {
    checkmate::assert_directory(x = args$path, access = "r", add = check_collection, .var.name = "path")
  }

  if (any(names(args) == "by")) {
    checkmate::assert_character(x = args$by, null.ok = TRUE, add = check_collection, .var.name = "by", len = 1)
  }

  if (any(names(args) == "macro.average")) {
    checkmate::assert_logical(x = args$macro.average, len = 1, add = check_collection, .var.name = "macro.average")
  }

  if (any(names(args) == "min.overlap")) {
    checkmate::assert_number(x = args$min.overlap, lower = 0.0001, upper = 1, add = check_collection, .var.name = "min.overlap")
  }

  if (any(names(args) == "filter")) {
    checkmate::assert_character(x = args$filter, null.ok = TRUE, add = check_collection, .var.name = "filter", len = 1)
  }

  if (any(names(args) == "envelopes")) {
    checkmate::assert_multi_class(x = args$envelopes, classes = c("envelope", "list"), null.ok = TRUE, add = check_collection, .var.name = "envelopes")
  }

  if (any(names(args) == "hop.size")) {
    checkmate::assert_number(x = args$hop.size, lower = 0.0001, add = check_collection, .var.name = "hop.size")
  }

  if (any(names(args) == "wl")) {
    checkmate::assert_number(x = args$wl, lower = 2, add = check_collection, .var.name = "wl")
  }

  if (any(names(args) == "bp")) {
    checkmate::assert_numeric(x = args$bp, any.missing = FALSE, all.missing = FALSE, len = 2, unique = TRUE, lower = 0, add = check_collection, .var.name = "bp")
  }

  if (any(names(args) == "smooth")) {
    checkmate::assert_numeric(x = args$smooth, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0.0001, add = check_collection, .var.name = "smooth")
  }

  if (any(names(args) == "threshold")) {
    if (as.character(fun)[[1]] %in% c("energy_detector", "optimize_energy_detector")) {
      checkmate::assert_numeric(x = args$threshold, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0.0001, upper = 99.9, add = check_collection, .var.name = "threshold")
    }

    if (as.character(fun)[[1]] %in% c("template_detector", "optimize_template_detector")) {
      checkmate::assert_numeric(x = args$threshold, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0.0001, upper = 0.999, add = check_collection, .var.name = "threshold")
    }
  }

  if (any(names(args) == "peak.amplitude")) {
    checkmate::assert_numeric(x = args$peak.amplitude, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0, add = check_collection, .var.name = "peak.amplitude")
  }

  if (any(names(args) == "thinning")) {
    checkmate::assert_numeric(x = args$thinning, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0.0001, upper = 1, add = check_collection, .var.name = "thinning")
  }

  if (any(names(args) == "min.duration")) {
    checkmate::assert_numeric(x = args$min.duration, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0, add = check_collection, .var.name = "min.duration")
  }

  if (any(names(args) == "max.duration")) {
    checkmate::assert_numeric(x = args$max.duration, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0.0001, add = check_collection, .var.name = "max.duration")
  }

  if (any(names(args) == "digits")) {
    checkmate::assert_integerish(args$digits, add = check_collection, lower = 0, upper = 16, .var.name = "digits")
  }

  if (any(names(args) == "units")) {
    checkmate::assert_character(x = args$units, null.ok = FALSE, add = check_collection, .var.name = "units", len = 2, any.missing = FALSE, all.missing = FALSE, ignore.case = TRUE)
    checkmate::assert_character(x = args$units, add = check_collection, .var.name = "units", any.missing = FALSE, all.missing = FALSE, pattern = "^Hz$|^kHz$|^s$|^ms$", ignore.case = TRUE)
  }

  if (any(names(args) == "normalize")) {
    checkmate::assert_logical(x = args$normalize, len = 1, add = check_collection, .var.name = "normalize")
  }

  if (any(names(args) == "acoustic.space")) {
    checkmate::assert_multi_class(x = args$acoustic.space, classes = c("data.frame", "matrix"), null.ok = TRUE, add = check_collection, .var.name = "envelopes")
  }

  if (any(names(args) == "n.sub.spaces")) {
    checkmate::assert_integerish(args$n.sub.spaces, add = check_collection, lower = 1, .var.name = "n.sub.spaces")
  }

  if (any(names(args) == "plot")) {
    checkmate::assert_logical(x = args$plot, len = 1, add = check_collection, .var.name = "plot")
  }

  if (any(names(args) == "color")) {
    checkmate::assert_character(x = args$color, add = check_collection, .var.name = "color", any.missing = FALSE, all.missing = FALSE)
  }

  if (any(names(args) == "wave")) {
    checkmate::assert_class(x = args$wave, classes = "Wave", null.ok = FALSE, add = check_collection, .var.name = "wave")
  }

  if (any(names(args) == "envelope")) {
    checkmate::assert_logical(x = args$envelope, len = 1, add = check_collection, .var.name = "envelope")
  }

  if (any(names(args) == "collevels")) {
    checkmate::assert_numeric(x = args$collevels, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0, add = check_collection, .var.name = "collevels")
  }

  if (any(names(args) == "palette")) {
    checkmate::assert_function(x = args$palette, null.ok = FALSE, add = check_collection, .var.name = "palette")
  }

  if (any(names(args) == "template.correlation")) {
    checkmate::assert_list(x = args$template.correlation, any.missing = FALSE, all.missing = FALSE, unique = TRUE, len = 4, add = check_collection, .var.name = "template.correlation")
  }

  if (any(names(args) == "template.correlations")) {
    checkmate::assert_multi_class(x = args$template.correlations, classes = c("template_correlations", "list"), null.ok = FALSE, add = check_collection, .var.name = "template.correlations")
  }

  if (any(names(args) == "line.x.position")) {
    checkmate::assert_numeric(x = args$line.x.position, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0, add = check_collection, .var.name = "line.x.position")
  }

  if (any(names(args) == "ovlp")) {
    checkmate::assert_numeric(x = args$ovlp, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0, upper = 99.9, len = 1, add = check_collection, .var.name = "ovlp")
  }

  if (any(names(args) == "sgmt.dur")) {
    checkmate::assert_numeric(x = args$sgmt.dur, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0.0001, len = 1, add = check_collection, .var.name = "sgmt.dur")
  }

  if (any(names(args) == "sgmts")) {
    checkmate::assert_integerish(x = args$sgmts, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 2, len = 1, add = check_collection, .var.name = "sgmts")
  }

  if (any(names(args) == "only.sels")) {
    checkmate::assert_logical(x = args$only.sels, len = 1, add = check_collection, .var.name = "only.sels")
  }

  if (any(names(args) == "fbtype")) {
    checkmate::assert_character(x = args$fbtype, add = check_collection, .var.name = "fbtype", any.missing = FALSE, len = 1, all.missing = FALSE, pattern = "^mel$|^bark$|^htkmel$|^fcmel$", ignore.case = TRUE)
  }

  if (any(names(args) == "type")) {
    checkmate::assert_character(x = args$type, null.ok = FALSE, add = check_collection, .var.name = "type", len = 1, any.missing = FALSE, all.missing = FALSE, ignore.case = TRUE)
    checkmate::assert_character(x = args$type, add = check_collection, .var.name = "type", any.missing = FALSE, all.missing = FALSE, pattern = "^fourier$|^mfcc$|^mel-auditory$", ignore.case = TRUE)
  }

  if (any(names(args) == "cor.method")) {
    checkmate::assert_character(x = args$cor.method, null.ok = FALSE, add = check_collection, .var.name = "cor.method", len = 1, any.missing = FALSE, all.missing = FALSE, ignore.case = TRUE)
    checkmate::assert_choice(x = args$cor.method, choices = c("pearson", "kendall", "spearman"), add = check_collection, .var.name = "cor.method")
  }

  if (any(names(args) == "wn")) {
    checkmate::assert_character(x = args$wn, null.ok = FALSE, add = check_collection, .var.name = "wn", len = 1, any.missing = FALSE, all.missing = FALSE, ignore.case = TRUE)
    checkmate::assert_choice(x = args$wn, choices = c("bartlett", "blackman", "flattop", "hamming", "hanning", "rectangle"), add = check_collection, .var.name = "wn")
  }

  if (any(names(args) == "verbose")) {
    checkmate::assert_logical(x = args$verbose, len = 1, add = check_collection, .var.name = "verbose")
  }

  if (any(names(args) == "mid.point")) {
    checkmate::assert_logical(x = args$mid.point, len = 1, add = check_collection, .var.name = "mid.point")
  }

  if (any(names(args) == "positions")) {
    checkmate::assert_numeric(x = args$positions, any.missing = FALSE, all.missing = FALSE, unique = TRUE, len = 2, add = check_collection, .var.name = "positions")
  }

  if (any(names(args) == "size")) {
    checkmate::assert_numeric(x = args$size, any.missing = FALSE, len = 1, add = check_collection, .var.name = "size")
  }

  return(check_collection)
}

# set options when loading package
.onLoad <- function(libname, pkgname) {
  options("ohun_check_args" = TRUE)
  invisible(NULL)
}


# remove options when unloading
.onUnload <- function(libpath) {
  options(ohun_check_args = NULL)
  invisible(NULL)
}


#### internal function to compute spectrogram cross-correlation
# create function to calculate correlation between 2 spectrograms
XC_FUN <- function(spc1, spc2, cm) {
  # define short and long envelope for sliding one (short) over the other (long)
  if (ncol(spc1) > ncol(spc2)) {
    lg.spc <- spc1
    shrt.spc <- spc2
  } else {
    lg.spc <- spc2
    shrt.spc <- spc1
  }

  # get length of shortest minus 1 (1 if same length so it runs a single correlation)
  shrt.lgth <- ncol(shrt.spc) - 1

  # steps for sliding one signal over the other
  stps <- ncol(lg.spc) - ncol(shrt.spc)

  # set sequence of steps, if <= 1 then just 1 step
  if (stps <= 1) {
    stps <- 1
  } else {
    stps <- 1:stps
  }

  # calculate correlations at each step
  cors <- vapply(stps, function(x, cor.method = cm) {
    warbleR::try_na(cor(
      c(lg.spc[, x:(x + shrt.lgth)]),
      c(shrt.spc),
      method = cm,
      use = "pairwise.complete.obs"
    ))
  }, FUN.VALUE = numeric(1))

  # make negative values 0
  cors[cors < 0] <- 0

  return(cors)
}


##############################################################################################################

#' print method for class \code{template_correlations}
#'
#' @param x Object of class \code{template_correlations}, generated by \code{\link{template_correlator}}.
#' @param ...	 further arguments passed to or from other methods. Ignored when printing 'template_correlations' class objects.
#' @keywords internal
#'
#' @export
#' @returns Prints a summary of an object of class 'template_correlations'.


print.template_correlations <- function(x, ...) {
  message2(paste(
    "Object of class",
    cli::style_bold("'template_correlations' \n")
  ))

  message2(
    color = "silver", x =
      paste(
        "* The output of the following",
        cli::style_italic("template_correlator()"),
        "call: \n"
      )
  )

  cll <- paste0(deparse(x$call_info$call))
  message2(color = "silver", x = cli::style_italic(gsub("    ", "", cll), "\n"))

  # get file and template names
  files_templates <- sapply(names(x)[-length(x)], strsplit, "/")
  templates <- unique(sapply(files_templates, "[[", 1))
  files <- unique(sapply(files_templates, "[[", 2))

  message2(
    color = "silver", x =
      paste(
        paste("* Contains", length(x) - 1, "correlation score vector(s) from"),
        length(templates),
        "template(s):\n",
        paste(cli::style_italic(utils::head(templates), collapse = " ")),
        if (length(templates) > 6) {
          paste("... and", length(templates) - 6, "more")
        } else {
          ""
        }
      )
  )

  message2(
    color = "silver", x =
      paste(
        paste("\n... and"),
        length(files),
        "sound files(s):\n",
        paste(cli::style_italic(utils::head(files), collapse = " ")),
        if (length(files) > 6) {
          paste("... and", length(files) - 6, "more")
        } else {
          ""
        }
      )
  )

  # print ohun version
  message2(
    color = "silver", x =
      paste0(
        "\n * Created by ",
        cli::style_bold("ohun "),
        x$call_info$ohun.version
      )
  )
}

### function to do energy detection of sound events (i is the file name) use internally by energy_detector
detect_FUN <-
  function(file,
           wl,
           thres,
           pa,
           min.dur,
           max.dur,
           pth,
           bpass,
           thin,
           smth,
           envlp,
           hop.siz,
           cors,
           pbar,
           hold.t) {
    # get envelope if not supplied
    if (is.null(envlp)) {
      envlp <- env_ohun_int(
        i = file,
        pth,
        bpass,
        hop.siz,
        wl,
        cors,
        thin,
        pbar,
        smth,
        normalize = TRUE
      )
    } else {
      envlp <- envlp[[file]]
    }

    # normalize to range
    if (max(envlp$envelope) > 1) {
      envlp$envelope <- envlp$envelope - min(envlp$envelope)
      envlp$envelope <- envlp$envelope / max(envlp$envelope)
    }

    # time interval between consecutive samples
    hop.samples <- envlp$duration / (length(envlp$envelope) - 1)

    # get times at which threshold is crossed
    cross_thresh <- unlist(lapply(2:length(envlp$envelope), function(x) {
      # positive means going up
      if (envlp$envelope[x] > thres & envlp$envelope[x - 1] <= thres) out <- hop.samples * (x - 1)
      # negative means going down
      if (envlp$envelope[x] <= thres & envlp$envelope[x - 1] > thres) out <- hop.samples * (x - 1) * -1
      # anything else should be null to save memory
      if (envlp$envelope[x] <= thres & envlp$envelope[x - 1] <= thres | envlp$envelope[x] > thres & envlp$envelope[x - 1] > thres) out <- NULL

      return(out)
    }))

    ## FIX IF START OR END OF sound eventS IS NOT INCLUDED IN SOUND FILE
    # get start and end of detections
    # starts are the positive ones
    starts <- cross_thresh[cross_thresh > 0]
    # and ends the negative ones (should be converted to positive)
    ends <- abs(cross_thresh[cross_thresh < 0])

    # if there is no end
    if (length(starts) > 0 & length(ends) == 0) ends <- envlp$duration

    # if there is no start
    if (length(ends) > 0 & length(starts) == 0) starts <- 0

    # if there are both starts and ends detected
    if (length(starts) > 0 & length(ends) > 0) {
      # if start is not lower in the first detection
      if (starts[1] > ends[1]) starts <- c(0, starts)

      # if end is not higher in the last
      if (starts[length(starts)] > ends[length(ends)]) ends <- c(ends, envlp$duration)

      # put time of detection in data frame
      detections_df <-
        data.frame(
          sound.files = file,
          duration = ends - starts,
          selec = NA,
          start = starts,
          end = ends,
          stringsAsFactors = FALSE
        )

      # add row names
      if (nrow(detections_df) > 0) {
        detections_df$selec <- seq_len(nrow(detections_df))
      }
    } else { # return NAs
      detections_df <-
        data.frame(
          sound.files = file,
          duration = NA,
          selec = NA,
          start = NA,
          end = NA,
          stringsAsFactors = FALSE
        )
    }

    # TIME FILTERS
    # if something was detected applied time filters
    if (nrow(detections_df) > 0) {
      ## HOLD TIME MERGING
      # merge selections based on hold time
      if (hold.t > 0 & nrow(detections_df) > 1) {
        # empty column to tag rows to be merged
        detections_df$ovlp.sels <- NA

        # calculate overlapping selection after adding hope time
        for (e in 1:(nrow(detections_df) - 1)) {
          # if overlap
          if (detections_df$end[e] + hold.t / 1000 >= detections_df$start[e + 1]) {
            # return 1 if is the first merging
            if (all(is.na(detections_df$ovlp.sels))) detections_df$ovlp.sels[c(e, e + 1)] <- 1

            # if current (e) overlapping with previous one
            if (is.na(detections_df$ovlp.sels[e])) {
              detections_df$ovlp.sels[c(e, e + 1)] <- max(detections_df$ovlp.sels, na.rm = TRUE) + 1
            }

            # if overlapping with previous one use same tag
            detections_df$ovlp.sels[e + 1] <- detections_df$ovlp.sels[e]
          }
        }

        # subset non-overlapping and overlapping
        no_ovlp <- detections_df[is.na(detections_df$ovlp.sels), ]
        ovlp <- detections_df[!is.na(detections_df$ovlp.sels), ]

        # if some overlaps detected
        if (nrow(ovlp) > 0) {
          # loop to merge selections
          out <- lapply(X = unique(ovlp$ovlp.sels), FUN = function(x) {
            # subset for one level
            Y <- ovlp[ovlp$ovlp.sels == x, ]

            # keep only one per overlapping group label
            Z <- Y[1, , drop = FALSE]

            # start is the minimum of all starts
            Z$start <- min(Y$start)

            # end is the maximum of all ends
            Z$end <- max(Y$end)

            return(Z)
          })

          # put list together in a data frame
          ovlp <- do.call(rbind, out)

          # add non-overlapping selections
          detections_df <- rbind(ovlp, no_ovlp)

          # order selections by sound file and time
          detections_df <- detections_df[order(detections_df$start), ]

          # relabel selec column
          detections_df$selec <- seq_len(nrow(detections_df))

          # recalculate duration (gets messed up when using hold time)
          detections_df$duration[!is.na(detections_df$start)] <- round(detections_df$end[!is.na(detections_df$start)] - detections_df$start[!is.na(detections_df$start)], 7)
        } else {
          detections_df <- no_ovlp
        } # if not return non-overlapping
      }

      # remove sound events based on duration
      if (min.dur > 0) {
        detections_df <- detections_df[detections_df$duration > min.dur / 1000, ]
      }
      if (max.dur < Inf) {
        detections_df <- detections_df[detections_df$duration < max.dur / 1000, ]
      }
    }

    # remove extra column
    detections_df$ovlp.sels <- NULL

    # measure peak.amplitude
    if (pa > 0) {
      detections_df <- warbleR::sound_pressure_level(detections_df, parallel = 1, path = pth, pb = FALSE, type = "peak")

      detections_df <- detections_df[detections_df$SPL > pa, ]

      # remove extra column
      detections_df$SPL <- NULL
    }
    return(detections_df)
  }


########################### internal function to get an envelope used by get_envelopes ###################

env_ohun_int <-
  function(i,
           path,
           bp,
           hop.size,
           wl,
           cores,
           thinning,
           pb,
           smooth,
           normalize) {
    # read wave object
    wave_obj <- warbleR::read_sound_file(X = i, path = path)

    # adjust wl based on hope.size (convert to samples)
    if (is.null(wl)) {
      wl <- round(wave_obj@samp.rate * hop.size / 1000, 0)
    }

    # make wl even if odd
    if (!(wl %% 2) == 0) wl <- wl + 1

    # convert smooth to samples
    smooth <- round(wave_obj@samp.rate * smooth / 1000, 0)

    # filter frequencies
    if (!is.null(bp)) {
      wave_obj <-
        seewave::ffilter(
          wave_obj,
          f = wave_obj@samp.rate,
          from = bp[1] * 1000,
          to = bp[2] * 1000,
          bandpass = TRUE,
          wl = wl,
          output = "Wave"
        )
    }

    # detect sound events based on amplitude (modified from seewave::timer function)
    amp_vector <- wave_obj@left

    # original number of samples
    n.samples <- length(amp_vector)

    # original duration
    wave_dur <- seewave::duration(wave_obj)

    # extract envelope
    envp <-
      warbleR::envelope(
        x = amp_vector,
        ssmooth = smooth
      )

    # flat edges (first and last 100 ms) if lower than lowest amp value
    if (n.samples > wave_obj@samp.rate / 5) {
      min.envp <- min(envp[(wave_obj@samp.rate / 10):(length(envp) - wave_obj@samp.rate / 5)])

      if (envp[1] < min.envp) envp[1:min(which(envp >= min.envp))] <- min.envp

      if (envp[length(envp)] < min.envp) envp[max(which(envp >= min.envp)):length(envp)] <- min.envp
    }

    # force to be in the range 0-1
    if (normalize) {
      envp <- envp - min(envp)
      envp <- envp / max(envp)
    }

    # thin
    if (thinning < 1) {
      if (n.samples * thinning < 10) stop2("thinning is too high, no enough samples left for at least 1 sound file")

      # reduce size of envelope
      envp <-
        stats::approx(
          x = seq(0, wave_dur, length.out = length(envp)),
          y = envp,
          n = round(n.samples * thinning),
          method = "linear"
        )$y
    }

    output <- list(
      envelope = envp,
      duration = wave_dur,
      org.n.samples = n.samples,
      sampling.freq = wave_obj@samp.rate
    )

    return(output)
  }


###
# function to get spectrogram matrices in template correlator
spc_FUN <-
  function(j,
           pth,
           W,
           hop,
           wlg,
           ovl,
           w,
           bndpss,
           nbnds,
           entire = FALSE,
           fbt,
           typ,
           ...) {
    # read entire sound file
    if (entire) {
      clp <- warbleR::read_sound_file(X = j, path = pth)
    } else {
      clp <- warbleR::read_sound_file(
        X = W,
        index = j,
        path = pth
      )
    }

    # adjust wl based on hope.size
    if (is.null(wlg)) {
      wlg <- round(clp@samp.rate * hop / 1000, 0)
    }

    # make wl even if odd
    if (!(wlg %% 2) == 0) {
      wlg <- wlg + 1
    }

    # steps for time bins
    steps <- seq(1, length(clp@left) - wlg, wlg - (ovl * wlg / 100))

    if (typ == "fourier") {
      spc <-
        warbleR:::stft_wrblr_int(
          wave = matrix(clp@left),
          f = clp@samp.rate,
          wl = wlg,
          zp = 0,
          step = steps,
          wn = w,
          fftw = FALSE,
          scale = TRUE,
          complex = FALSE
        )

      # calculate freq values for spc rows
      freq <-
        seq(0,
          (clp@samp.rate / 2) - (clp@samp.rate / wlg),
          length.out = nrow(spc)
        ) / 1000

      # apply bandpass
      spc <- spc[freq >= bndpss[1] & freq <= bndpss[2], ]

      # log amplitude values
      spc <- 20 * log10(spc)
    } else {
      # calculate MFCCs
      melfcc_output <-
        melfcc(
          clp,
          hoptime = (steps[2] - steps[1]) / clp@samp.rate,
          wintime = wlg / clp@samp.rate,
          fbtype = fbt,
          spec_out = TRUE,
          frames_in_rows = FALSE,
          minfreq = bndpss[1] * 1000,
          maxfreq = bndpss[2] * 1000,
          ...
        )

      if (typ == "mfcc") {
        spc <- melfcc_output$cepstra
      }

      if (typ == "mel-auditory") {
        spc <- melfcc_output$aspectrum
      }

      # log amplitude values
      spc <- 20 * log10(spc + abs(min(spc)) + 10)
    }

    # replace inf by NA
    spc[is.infinite(spc)] <- NA

    return(spc)
  }
