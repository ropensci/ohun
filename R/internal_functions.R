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



# internal function to find (and plot) templates in the acoustic space
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
  
  if (!is.null(df)){
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
  } else
    df <-  data.frame(sound.files = vector(), detection.id = vector(), reference.id = vector(), IoU = vector())
  
  return(df)
}

.onAttach <- 
  function(libname, pkgname) {
    packageStartupMessage("\nPlease cite as: \n")
    packageStartupMessage(" Araya-Salas, M. (2022), ohun: diagnosing and optimizing automated sound event detection.")
    packageStartupMessage(" R package version 0.1.1 https://CRAN.R-project.org/package=ohun \n")
  }


# custom assert functions to check arguments
# Simple custom check function  no duplicated selection labels
check_unique_sels <- function(x, fun) 
  if (fun == "label_detection"){
    if (anyDuplicated(paste(x$sound.files, x$selec, x$template)) > 0) "Duplicated 'selec' labels within at least one combination of sound file/template" else TRUE} else {
      if (anyDuplicated(paste(x$sound.files, x$selec)) > 0) "Duplicated 'selec' labels within at least one sound file" else TRUE}

assert_unique_sels <- checkmate::makeAssertionFunction(check_unique_sels)

## function to check arguments
check_arguments <- function(fun, args){
  
  # create object to store check results
  check_collection <- checkmate::makeAssertCollection()
  
  ### check arguments
  if (any(names(args) == "reference")){
    checkmate::assert_multi_class(x = args$reference, classes = c("data.frame", "selection.table"), add = check_collection, .var.name = "reference")
    checkmate::assert_names(x = names(args$reference), type = "unique", must.include = c("sound.files", "selec", "start", "end"), add = check_collection, .var.name = "names(reference)")
    try(checkmate::assert_data_frame(x = args$reference[, c("sound.files", "selec", "start", "end")], any.missing = FALSE, add = check_collection, .var.name = "reference"), silent = TRUE)
    assert_unique_sels(x = args$reference, fun = fun,  add = check_collection, .var.name = "reference")
  }
  
  if (any(names(args) == "detection")){
    checkmate::assert_data_frame(x = args$detection, any.missing = TRUE, add = check_collection, .var.name = "detection")
    checkmate::assert_multi_class(x = args$detection, classes = c("data.frame", "selection.table"), add = check_collection, .var.name = "detection")
    
    cols <- if(fun == "consensus_detection") c("sound.files", "selec", "start", "end", "detection.class") else
      c("sound.files", "selec", "start", "end")
    
    checkmate::assert_names(x = names(args$detection), type = "unique", must.include = cols, add = check_collection, .var.name = "names(detection)")
    try(checkmate::assert_data_frame(x = args$detection[, cols], any.missing = TRUE, add = check_collection, .var.name = "detection"), silent = TRUE)
    
    assert_unique_sels(x = args$detection, fun = fun,  add = check_collection, .var.name = "detection")
  }
  
  if (any(names(args) == "X")){
    checkmate::assert_multi_class(x = args$X, classes = c("data.frame", "selection.table"), add = check_collection, .var.name = "X")
    checkmate::assert_names(x = names(args$X), type = "unique", must.include = c("sound.files", "selec", "start", "end"), add = check_collection, .var.name = "names(X)")
    try(checkmate::assert_data_frame(x = args$X[, c("sound.files", "selec", "start", "end")], any.missing = FALSE, add = check_collection, .var.name = "X"), silent = TRUE)
  }
  
  if (any(names(args) == "templates")){
    checkmate::assert_multi_class(x = args$templates, classes = c("data.frame", "selection.table", "extended.selection.table"), add = check_collection, .var.name = "templates")
    checkmate::assert_names(x = names(args$templates), type = "unique", must.include = c("sound.files", "selec", "start", "end"), add = check_collection, .var.name = "names(templates)")
    try(checkmate::assert_data_frame(x = args$templates[, c("sound.files", "selec", "start", "end")], any.missing = FALSE, add = check_collection, .var.name = "templates"), silent = TRUE)
    assert_unique_sels(x = args$templates, fun = fun,  add = check_collection, .var.name = "templates")
  }
  
  if (any(names(args) == "previous.output"))
    checkmate::assert_data_frame(x = args$previous.output, any.missing = TRUE, add = check_collection, .var.name = "previous.output")
  
  if (any(names(args) == "diagnostic"))
    checkmate::assert_data_frame(x = args$diagnostic, any.missing = TRUE, add = check_collection, .var.name = "diagnostic")
  
  # if (any(names(args) == "files"))
  #   if (!is.null(args$files))
  #   checkmate::assert_file_exists(x = args$files, access = "r", extension = c("mp3", "wav", "wac", "flac"), add = check_collection, .var.name = "files")
  
  if (any(names(args) == "by.sound.file"))
    checkmate::assert_logical(x = args$by.sound.file, len = 1, add = check_collection, .var.name = "by.sound.file")
  
  if (any(names(args) == "time.diagnostics"))
    checkmate::assert_logical(x = args$time.diagnostics, len = 1, add = check_collection, .var.name = "time.diagnostics")
  
  if (any(names(args) == "cores"))
    checkmate::assert_integerish(args$cores, add = check_collection, lower = 1, upper = parallel::detectCores(), .var.name = "cores")
  
  if (any(names(args) == "pb"))
    checkmate::assert_logical(x = args$pb, len = 1, add = check_collection, .var.name = "pb")
  
  if (any(names(args) == "path"))
    checkmate::assert_directory(x = args$path, access = "r", add = check_collection, .var.name = "path")
  
  if (any(names(args) == "by"))
    checkmate::assert_character(x = args$by, null.ok = TRUE, add = check_collection, .var.name = "by", len = 1)
  
  if (any(names(args) == "macro.average"))
    checkmate::assert_logical(x = args$macro.average, len = 1, add = check_collection, .var.name = "macro.average")
  
  if (any(names(args) == "min.overlap"))
    checkmate::assert_number(x = args$min.overlap, lower = 0.0001, upper = 1, add = check_collection, .var.name = "min.overlap")
  
  if (any(names(args) == "filter"))
    checkmate::assert_character(x = args$filter, null.ok = TRUE, add = check_collection, .var.name = "filter", len = 1)
  
  if (any(names(args) == "envelopes"))
    checkmate::assert_multi_class(x = args$envelopes, classes = c("envelope", "list"), null.ok = TRUE, add = check_collection, .var.name = "envelopes")
  
  if (any(names(args) == "hop.size"))
    checkmate::assert_number(x = args$hop.size, lower = 0.0001, add = check_collection, .var.name = "hop.size")
  
  if (any(names(args) == "wl"))
    checkmate::assert_number(x = args$wl, lower = 2, add = check_collection, .var.name = "wl")
  
  if (any(names(args) == "bp"))
    checkmate::assert_numeric(x = args$bp, any.missing = FALSE, all.missing = FALSE, len = 2, unique = TRUE, lower = 0, add = check_collection, .var.name = "bp")
  
  if (any(names(args) == "smooth"))
    checkmate::assert_numeric(x = args$smooth, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0.0001, add = check_collection, .var.name = "smooth")
  
  if (any(names(args) == "threshold")){
    if (as.character(fun) %in% c("energy_detector", "optimize_energy_detector"))
      checkmate::assert_numeric(x = args$threshold, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0.0001, upper = 99.9, add = check_collection, .var.name = "threshold")
    if (as.character(fun) %in% c("template_detector", "optimize_template_detector"))
      checkmate::assert_numeric(x = args$threshold, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0.0001, upper = 0.999, add = check_collection, .var.name = "threshold")
  }
  
  if (any(names(args) == "thinning"))
    checkmate::assert_numeric(x = args$thinning, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0.0001, upper = 1, add = check_collection, .var.name = "thinning")
  
  if (any(names(args) == "min.duration"))
    checkmate::assert_numeric(x = args$min.duration, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0, add = check_collection, .var.name = "min.duration")
  
  if (any(names(args) == "max.duration"))
    checkmate::assert_numeric(x = args$max.duration, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0.0001, add = check_collection, .var.name = "max.duration")
  
  if (any(names(args) == "digits"))
    checkmate::assert_integerish(args$digits, add = check_collection, lower = 0, upper = 16, .var.name = "digits")
  
  if (any(names(args) == "units")){
    checkmate::assert_character(x = args$units, null.ok = FALSE, add = check_collection, .var.name = "units", len = 2, any.missing = FALSE, all.missing = FALSE, ignore.case = TRUE)
    checkmate::assert_character(x = args$units, add = check_collection, .var.name = "units", any.missing = FALSE, all.missing = FALSE, pattern = "^Hz$|^kHz$|^s$|^ms$", ignore.case = TRUE)
  }
  
  if (any(names(args) == "normalize"))
    checkmate::assert_logical(x = args$normalize, len = 1, add = check_collection, .var.name = "normalize")
  
  if (any(names(args) == "acoustic.space"))
    checkmate::assert_multi_class(x = args$acoustic.space, classes = c("data.frame", "matrix"), null.ok = TRUE, add = check_collection, .var.name = "envelopes")
  
  if (any(names(args) == "n.sub.spaces"))
    checkmate::assert_integerish(args$n.sub.spaces, add = check_collection, lower = 1, .var.name = "n.sub.spaces") 
  
  if (any(names(args) == "plot"))
    checkmate::assert_logical(x = args$plot, len = 1, add = check_collection, .var.name = "plot")
  
  if (any(names(args) == "color"))
    checkmate::assert_character(x = args$color, add = check_collection, .var.name = "color", any.missing = FALSE, all.missing = FALSE)
  
  if (any(names(args) == "wave"))
    checkmate::assert_class(x = args$wave, classes = "Wave", null.ok = FALSE, add = check_collection, .var.name = "wave")
  
  if (any(names(args) == "envelope"))
    checkmate::assert_logical(x = args$envelope, len = 1, add = check_collection, .var.name = "envelope")
  
  if (any(names(args) == "collevels"))
    checkmate::assert_numeric(x = args$collevels, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0, add = check_collection, .var.name = "collevels")
  
  if (any(names(args) == "palette"))
    checkmate::assert_function(x = args$palette, null.ok = FALSE, add = check_collection, .var.name = "palette")
  
  if (any(names(args) == "template.correlation"))
    checkmate::assert_list(x = args$template.correlation, any.missing = FALSE, all.missing = FALSE, unique = TRUE, len = 4, add = check_collection, .var.name = "template.correlation")
  
  if (any(names(args) == "template.correlations"))
    checkmate::assert_multi_class(x = args$template.correlations, classes = c("template_correlations", "list"), null.ok = FALSE, add = check_collection, .var.name = "template.correlations")
  
  if (any(names(args) == "line.x.position"))
    checkmate::assert_numeric(x = args$line.x.position, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0, add = check_collection, .var.name = "line.x.position")
  
  if (any(names(args) == "ovlp"))
    checkmate::assert_numeric(x = args$ovlp, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0, upper = 99.9, len = 1, add = check_collection, .var.name = "ovlp")
  
  if (any(names(args) == "sgmt.dur"))
    checkmate::assert_numeric(x = args$sgmt.dur, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 0.0001, len = 1, add = check_collection, .var.name = "sgmt.dur")
  
  if (any(names(args) == "sgmts"))
    checkmate::assert_integerish(x = args$sgmts, any.missing = FALSE, all.missing = FALSE, unique = TRUE, lower = 2, len = 1, add = check_collection, .var.name = "sgmts")
  
  if (any(names(args) == "only.sels"))
    checkmate::assert_logical(x = args$only.sels, len = 1, add = check_collection, .var.name = "only.sels")
  
  if (any(names(args) == "fbtype")){
    checkmate::assert_character(x = args$fbtype, add = check_collection, .var.name = "fbtype", any.missing = FALSE, len = 1, all.missing = FALSE, pattern = "^mel$|^bark$|^htkmel$|^fcmel$", ignore.case = TRUE)
  }
  
  if (any(names(args) == "type")){
    checkmate::assert_character(x = args$type, null.ok = FALSE, add = check_collection, .var.name = "type", len = 1, any.missing = FALSE, all.missing = FALSE, ignore.case = TRUE)
    checkmate::assert_character(x = args$type, add = check_collection, .var.name = "type", any.missing = FALSE, all.missing = FALSE, pattern = "^fourier$|^mfcc$|^mel-auditory$", ignore.case = TRUE)
  }
  
  if (any(names(args) == "cor.method")){
    checkmate::assert_character(x = args$cor.method, null.ok = FALSE, add = check_collection, .var.name = "cor.method", len = 1, any.missing = FALSE, all.missing = FALSE, ignore.case = TRUE)
    checkmate::assert_character(x = args$cor.method, add = check_collection, .var.name = "cor.method", any.missing = FALSE, all.missing = FALSE, pattern = "^pearson$|^kendall$|^spearman$", ignore.case = TRUE)
  }
  
  if (any(names(args) == "wn")){
    checkmate::assert_character(x = args$wn, null.ok = FALSE, add = check_collection, .var.name = "wn", len = 1, any.missing = FALSE, all.missing = FALSE, ignore.case = TRUE)
    checkmate::assert_character(x = args$wn, add = check_collection, .var.name = "wn", any.missing = FALSE, all.missing = FALSE, pattern = "bartlett|blackman|flattop|hamming|hanning|rectangle", ignore.case = TRUE)
  }
  
  if (any(names(args) == "verbose"))
    checkmate::assert_logical(x = args$verbose, len = 1, add = check_collection, .var.name = "verbose")
  
  if (any(names(args) == "mid.point"))
    checkmate::assert_logical(x = args$mid.point, len = 1, add = check_collection, .var.name = "mid.point")
  
  if (any(names(args) == "positions"))
    checkmate::assert_numeric(x = args$positions, any.missing = FALSE, all.missing = FALSE, unique = TRUE, len = 2, add = check_collection, .var.name = "positions")
  
  if (any(names(args) == "size"))
    checkmate::assert_numeric(x = args$size, any.missing = FALSE, len = 1, add = check_collection, .var.name = "size")
  
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

