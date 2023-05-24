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
