#' @title Find templates more similar to other signals in a reference table
#'
#' @description \code{get_templates} find the signals that are closer to the acoustic space  centroid (i.e. close to the average acoustic structure) in a reference table.
#' @usage get_templates(reference, acoustic.space = NULL, variance.cutoff = 0.7, path = ".",
#' n.templates = 1, ...)
#' @param reference Selection table (using the warbleR package's format, see \code{\link[warbleR]{selection_table}}) or data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end).
#' @param acoustic.space Numeric matrix or data frame with the dimensions of a custom acoustic space to be used for finding templates. if not supplied the acoustic space is calculated internally (default). Optional.
#' @param variance.cutoff Numeric vector of length 1 with the variance cutoff (> 0 & <= 1) for choosing which principal components will be used as acoustic space dimensions. Default is 0.7. Only used if 'acoustic.space' is not supplied.
#' @param path Character string containing the directory path where the sound files are located.
#'The current working directory is used as default.
#'@param n.templates Integer vector of length 1 with the number of templates to be returned. If 1 only the signal closer to the centroid is returned. If higher than 1 the function returns additional signals that are representative of the acoustic space, defined as those equally spaced across the distribution of distances to the centroid.
#' @param ... Additional arguments to be passed to \code{\link[warbleR]{spectro_analysis}} for further customization when measuring parameters to calculate the acoustic space.
#' @return The function returns a 'selection_table' (warbleR package's formats, see \code{\link[warbleR]{selection_table}}) or data frame (if sound files can't be found) containing the start and end of each signal by
#'   sound file. If no signal was detected for a sound file it is not included in the output data frame.
#' @export
#' @name get_templates
#' @details This function finds signals that are representative of the acoustic variation of the signals in a reference table. This is done by finding the signals closer to the centroid of the acoustic space. If the acoustic space is not supplied ('acoustic.space' argument) then the function will estimate it by measuring several acoustic parameters using the function \code{\link[warbleR]{spectro_analysis}} and summarizing it with Principal Component Analysis (after z-transforming parameters) using the function \code{\link[stats]{prcomp}}. The rationale here is that a signal closest to the average signal structure is more likely to share structural features with most signals across the acoustic space than a signal in the periphery of the space.
#' If only 1 template is required the function returns that closest to the acoustic space centroid. If more than 1 templated is required additional signals are returned that are representative of the acoustic space, defined as those equally spaced across the distribution of distances to the centroid. A column 'distance.quantile' is included in the output selection table that identifies the quantile of the distance to the centroid that each selection represents. Custom acoustic spaces can be supplied with argument 'acoustic.space'.
#'
#' @examples {
#' # Save example files into temporary working directory
#' data("lbh1", "lbh2", "lbh_reference")
#' writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
#' writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
#'
#' # get a single mean template
#' template <- get_templates(reference = lbh_reference, path = tempdir())
#'
#' # get 3 templates
#' template <- get_templates(reference = lbh_reference, n.templates = 3, path = tempdir())
#' }
#'
#' @references {
#' Araya-Salas, M. (2021), ohun: automatic detection of acoustic signals. R package version 0.1.0.
#' }
#' @seealso \code{\link{template_detector}}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}). Implements a
#' modified version of the timer function from seewave.
#last modification on feb-2022 (MAS)

get_templates <-
  function(reference,
           acoustic.space = NULL,
           variance.cutoff = 0.7,
           path = ".",
           n.templates = 1,
           ...) {
    if (!is.null(acoustic.space))
      if (nrow(reference) != nrow(acoustic.space))
        stop("'reference' and 'acoustic.space' must have the same number of columns")

    if (is.null(acoustic.space)) {
      spectral_parameters <- spectro_analysis(reference, path = path, ...)

      # remove columns with NAs
      spectral_parameters <-
        spectral_parameters[, !sapply(spectral_parameters, anyNA)]

      # get PCA
      pca <-
        stats::prcomp(spectral_parameters[, 2:27], scale. = TRUE)

      # get variance by PC
      variance_pca <- summary(pca)$importance

      # number of PCs to keep
      pcs_keep <- sum(variance_pca[3, ] <= variance.cutoff) + 1

      # print info
      cat(crayon::silver(
        paste0(
          pcs_keep,
          " principal components were kept which explained ",
          round(variance_pca[3, pcs_keep], 2),
          " of the variance"
        )
      ))

      # keep those as acoustic space
      acoustic.space <- pca$x[, 1:pcs_keep]
    }

    # get centroid
    centroid_coors <- colMeans(acoustic.space)

    # and istance to centroid
    dists_to_centroid <-
      unlist(warbleR:::pblapply_wrblr_int(pbar = FALSE, 1:nrow(acoustic.space), function(x)
        dist(
          rbind(acoustic.space[x, ], centroid_coors)
        )))

    # get quantile distribution of distances from centroid
    dist_quantiles <-
      stats::quantile(
        x = dists_to_centroid,
        probs = seq(0, 1, length.out = n.templates + 1)[1:(n.templates)],
        digits = 1
      )

    # get indices of selections closer to those quantiles
    template_indx <-
      sapply(dist_quantiles, function(x)
        which.min(abs(dists_to_centroid - x)))

    # extract those selections
    templates <- reference[template_indx, ]
    templates$distance.quantile <- names(template_indx)

    # save as a selection table
    templates <-
      warbleR::selection_table(templates, path = path, pb = FALSE)

    return(templates)
  }
