#' @title Find templates representative of the structural variation of sound events
#'
#' @description \code{get_templates} find the sound events that are closer to the acoustic space centroid (i.e. close to the average acoustic structure) in a reference table.
#' @param reference Selection table (using the warbleR package's format, see \code{\link[warbleR]{selection_table}}) or data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of sound event
#' (start and end).
#' @param acoustic.space Numeric matrix or data frame with the two dimensions of a custom acoustic space to be used for finding templates. if not supplied the acoustic space is calculated internally (default). Optional. Note that the function assumes that 'reference' and 'acoustic.space' refer to the same sound events and similarly ordered.
#' @param path Character string containing the directory path where the sound files are located.
#' The current working directory is used as default.
#' @param n.sub.spaces Integer vector of length 1 with the number of sub-spaces to split the total acoustic space. If \code{n.sub.spaces = 1}, only the sound event closer to the centroid is returned. If \code{n.sub.spaces > 1} the function returns additional sound events, corresponding to those closer to the centroids of the sub-spaces. To do this, the function defines sub-spaces as equal-size slices of a circle centered at the centroid of the acoustic space.
#' @param plot Logical to control if the plot is created. Default is \code{TRUE}.
#' @param color Character string with the point color. Default is '#21908C4D'.
#' @param ... Additional arguments to be passed to \code{\link[warbleR]{spectro_analysis}} for further customization when measuring parameters to calculate the acoustic space.
#' @return The function returns a 'selection_table' (warbleR package's formats, see \code{\link[warbleR]{selection_table}}) or data frame (if sound files can't be found) containing the start and end of each sound event by
#'   sound file.
#' @export
#' @name get_templates
#' @details This function finds sound events (from a reference table) that are representative of the acoustic structure variation of all sound events. This is done by finding the events closer to the centroid of the acoustic space. If the acoustic space is not supplied ('acoustic.space' argument) then the function will estimate it by measuring several acoustic features using the function \code{\link[warbleR]{spectro_analysis}} (features related to energy distribution in the frequency and time domain as well as features of the dominant frequency contours, see \code{\link[warbleR]{spectro_analysis}} for more details) and summarizing it with Principal Component Analysis (after z-transforming parameters) using the function \code{\link[stats]{prcomp}}. Acoustic features with missing values are removed before estimating Principal Component Analysis. The rationale is that a sound event close to the average structure is more likely to share structural features with most events across the acoustic space than a sound event in the periphery of the space.
#' If only 1 template is required the function returns the sound event closest to the acoustic space centroid. If more than 1 template is required additional sound events are returned that are representative of the acoustic space. To do this, the function defines sub-spaces as equal-size slices of a circle centered at the centroid of the acoustic space. A column 'template' is included in the output selection table that identifies each template. Custom acoustic spaces can be supplied with argument 'acoustic.space'. Notice that the function aims to partition spaces in which sounds are somehow homogeneously distributed. When clear clusters are found in the distribution of the acoustic space thus clusters might not match the sub-spaces defined by the function.
#'
#' @examples {
#'   # Save example files into temporary working directory
#'   data("lbh1", "lbh2", "lbh_reference")
#'   tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
#'   tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
#'
#'   # get a single mean template
#'   template <- get_templates(reference = lbh_reference, path = tempdir())
#'
#'   # get 3 templates
#'   template <- get_templates(reference = lbh_reference, n.sub.spaces = 3, path = tempdir())
#' }
#'
#' @references 
#'  Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C., Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. (2023). ohun: An R package for diagnosing and optimizing automatic sound event detection. Methods in Ecology and Evolution, 14, 2259–2271. https://doi.org/10.1111/2041-210X.14170
#' 
#' @seealso \code{\link{template_detector}}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}). Implements a
#' modified version of the timer function from seewave.

get_templates <-
  function(reference,
           acoustic.space = NULL,
           path = ".",
           n.sub.spaces = 1,
           plot = TRUE,
           color = "#21908C4D",
           ...) {
    # check arguments
    arguments <- as.list(base::match.call())

    # add objects to argument names
    for (i in names(arguments)[-1]) {
      arguments[[i]] <- get(i)
    }

    # check each arguments
    check_results <- check_arguments(fun = arguments[[1]], args = arguments)

    # report errors
    checkmate::reportAssertions(check_results)

    if (!is.null(acoustic.space)) {
      if (nrow(reference) != nrow(acoustic.space)) {
        stop2("'reference' and 'acoustic.space' must have the same number of columns")
      }
    }

    if (is.null(acoustic.space)) {
      spectral_parameters <- spectro_analysis(reference, path = path, ...)

      # remove columns with NAs
      spectral_parameters <-
        spectral_parameters[, !vapply(spectral_parameters, anyNA, FUN.VALUE = logical(1))]

      # remove columns with 0 variance
      spectral_parameters <-
        spectral_parameters[, vapply(spectral_parameters, function(x) if (is(x, "numeric")) var(x) else 1, FUN.VALUE = numeric(1)) > 0]

      # get PCA
      pca <-
        stats::prcomp(spectral_parameters[, 3:ncol(spectral_parameters)], scale. = TRUE)

      # get variance by PC
      variance_pca <- summary(pca)$importance

      # print info
      message2(
        color = "silver", x =
          paste0(
            "The first 2 principal components explained ",
            round(variance_pca[3, 2], 2),
            " of the variance"
          )
      )

      # keep those as acoustic space
      acoustic.space <- pca$x[, 1:2]

      plot_labs <- c("PC1", "PC2")
    } else {
      if (length(dim(acoustic.space)) != 2) {
        stop2("Acoustic space must be either a data frame or a matrix with 2 column")
      }

      if (ncol(acoustic.space) != 2) {
        stop2("Acoustic space must have 2 columns")
      }

      plot_labs <- if (!is.null(colnames(acoustic.space))) {
        colnames(acoustic.space)
      } else {
        c("Dimension 1", "Dimension 2")
      }
    }

    template_indx <- find_templates(reference = reference, n.sub.spaces = n.sub.spaces, space = acoustic.space, plot = plot, color = color, xlab = plot_labs[1], ylab = plot_labs[2])

    template_indx <- template_indx[!is.na(template_indx)]

    # extract those selections
    templates <- reference[template_indx, ]
    templates$template <- names(template_indx)

    return(templates)
  }
