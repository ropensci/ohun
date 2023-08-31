#' @title  Extract absolute amplitude envelopes
#'
#' @description \code{get_envelopes} extracts absolute amplitude envelopes to speed up energy detection
#' @usage get_envelopes(path = ".", files = NULL, bp = NULL, hop.size = 11.6, wl = NULL,
#' cores = 1, thinning = 1, pb = TRUE, smooth = 5, normalize = TRUE)
#' @param path Character string containing the directory path where the sound files are located.
#' The current working directory is used as default.
#' @param files character vector or indicating the sound files that will be analyzed. Supported file formats:'.wav', '.mp3', '.flac' and '.wac'. If not supplied the function will work on all sound files (in the supported format) in 'path'.
#' @param bp Numeric vector of length 2 giving the lower and upper limits of a
#'   frequency bandpass filter (in kHz). Default is \code{NULL}. Bandpass is done using the function code{\link[seewave]{ffilter}}, which applies a short-term Fourier transformation to first create a spectrogram in which the target frequencies are filtered and then is back transform into a wave object using a reverse Fourier transformation.
#' @param hop.size A numeric vector of length 1 specifying the time window duration (in ms). Default is 11.6 ms, which is equivalent to 512 wl for a 44.1 kHz sampling rate. Ignored if 'wl' is supplied.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram. Default is \code{NULL}. If supplied, 'hop.size' is ignored. Used internally for bandpass filtering (so only applied when 'bp' is supplied).
#' @param cores Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param thinning Numeric vector of length 1 in the range 0~1 indicating the proportional reduction of the number of
#' samples used to represent amplitude envelopes (i.e. the thinning of the envelopes). Usually amplitude envelopes have many more samples
#' than those needed to accurately represent amplitude variation in time, which affects the size of the
#' output (usually very large R objects / files). Default is \code{1} (no thinning). Higher sampling rates can afford higher size reduction (e.g. lower thinning values). Reduction is conducted by linear interpolation using \code{\link[stats]{approx}}. Note that thinning may decrease time precision and that the higher the thinning the less precise the time detection. It's generally not advised if no smoothing ('smooth' argument) is applied.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param smooth A numeric vector of length 1 to smooth the amplitude envelope
#'   with a sum smooth function. It controls the time 'neighborhood' (in ms) in which amplitude samples are smoothed (i.e. averaged with neighboring samples). Default is 5. 0 means no smoothing is applied. Note that smoothing is applied before thinning (see 'thinning' argument). The function  \code{\link[warbleR]{envelope}} is used internally which is analogous to sum smoothing in code{\link[seewave]{env}}. This argument is used internally by \code{\link{get_envelopes}}.
#' @param normalize Logical argument to control if envelopes are normalized to a 0-1 range.
#' @return An object of class 'envelopes'.
#' @export
#' @name get_envelopes
#' @details This function extracts the absolute amplitude envelopes of sound files. Can be used to manipulate envelopes before running \code{\link{energy_detector}}.
#'
#' @examples {
#'   # Save to temporary working directory
#'   data(list = c("lbh1", "lbh2"))
#'   tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
#'   tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
#'
#'   # get raw absolute amplitude envelopes
#'   envs <- get_envelopes(path = tempdir())
#'
#'   # extract segment for the first sound event in the first sound file
#'   x <- envs[[1]]$envelope
#'
#'   # and plot it
#'   plot(x[(length(x) / 9):(length(x) / 4)], type = "l", xlab = "samples", ylab = "amplitude")
#'
#'   # smoothing envelopes
#'   envs <- get_envelopes(path = tempdir(), smooth = 6.8)
#'   x <- envs[[1]]$envelope
#'   plot(x[(length(x) / 9):(length(x) / 4)], type = "l", xlab = "samples", ylab = "amplitude")
#'
#'   # smoothing and thinning
#'   envs <- get_envelopes(path = tempdir(), thinning = 1 / 10, smooth = 6.8)
#'   x <- envs[[1]]$envelope
#'   plot(x[(length(x) / 9):(length(x) / 4)], type = "l", xlab = "samples", ylab = "amplitude")
#'
#'   # no normalization
#'   envs <- get_envelopes(path = tempdir(), thinning = 1 / 10, smooth = 6.8)
#'   x <- envs[[1]]$envelope
#'   plot(x[(length(x) / 9):(length(x) / 4)],
#'     type = "l", xlab = "samples", ylab = "amplitude",
#'     normalize = FALSE
#'   )
#' }
#'
#' @references {
#' Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C., Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. 2022. ohun: an R package for diagnosing and optimizing automatic sound event detection. BioRxiv, 2022.12.13.520253. https://doi.org/10.1101/2022.12.13.520253
#' }
#' @seealso \code{\link{energy_detector}}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}).

get_envelopes <-
  function(path = ".",
           files = NULL,
           bp = NULL,
           hop.size = 11.6,
           wl = NULL,
           cores = 1,
           thinning = 1,
           pb = TRUE,
           smooth = 5,
           normalize = TRUE) {
    # check arguments
    if (options("ohun_check_args")$ohun_check_args) {
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
    }

    # return error if not all sound files were found
    if (is.null(files)) {
      files <- list.files(
        path = path,
        pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$",
        ignore.case = TRUE
      )
    } else if (!all(files %in% list.files(
      path = path,
      pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$",
      ignore.case = TRUE
    ))) {
      stop2("Some (or all) sound files were not found")
    }

    if (length(files) == 0) {
      stop2("no sound files found in working directory or 'path' supplied")
    }

    # Apply over each sound file
    # set clusters for windows OS
    if (Sys.info()[1] == "Windows" & cores > 1) {
      cl <- parallel::makeCluster(cores)
    } else {
      cl <- cores
    }

    # run function over sound files or selections in loop
    env_list <- warbleR:::pblapply_wrblr_int(
      pbar = pb,
      X = files,
      cl = cl,
      FUN = function(i) {
        env_ohun_int(
          i,
          path,
          bp,
          hop.size,
          wl,
          cores,
          thinning,
          pb,
          smooth,
          normalize
        )
      }
    )

    names(env_list) <- files

    # append call info
    env_list[[length(env_list) + 1]] <- list(
      parameters = lapply(as.list(base::match.call())[-1], eval),
      call = base::match.call(),
      ohun.version = packageVersion("ohun")
    )

    names(env_list)[length(env_list)] <- "call_info"

    # add class envelopes
    class(env_list) <- c("list", "envelopes")

    return(env_list)
  }


##############################################################################################################


#' Class 'envelopes': list of absolute amplitude envelopes
#'
#' Class for absolute amplitude envelopes
#' @export
#' @details An object of class \code{envelopes} created by \code{\link{get_envelopes}} is a list with sound files absolute amplitude envelopes and metadata
#' @seealso \code{\link{get_envelopes}}


##############################################################################################################

#' print method for class \code{envelopes}
#'
#' @param x Object of class \code{envelopes}, generated by \code{\link{get_envelopes}}.
#' @param ...	 further arguments passed to or from other methods. Ignored when printing envelopes.
#' @keywords internal
#'
#' @export
#' @returns Prints a summary of an object of class 'envelopes'.

print.envelopes <- function(x, ...) {
  message2(paste("Object of class", cli::style_bold("'envelopes' \n")))

  message2(color = "silver", x = paste("* The output of the following", cli::style_italic("get_envelopes()"), "call: \n"))

  cll <- paste0(deparse(x$call_info$call))
  message2(color = "silver", x = cli::style_italic(gsub("    ", "", cll), "\n"))

  file_names <- names(x)[-length(x)]

  message2(color = "silver", x = paste(paste("* Contains the amplitude envelopes of"), length(x) - 1, "sound file(s):\n", paste(cli::style_italic(utils::head(file_names), collapse = " ")), if (length(file_names) > 6) paste("... and", length(file_names) - 6, "more") else ""))

  # add message about amplitude envelope modifications
  if (any(names(x$call_info$parameters) == "smooth")) {
    if (x$call_info$parameters$smooth > 0) {
      smooth_message <- paste0(x$call_info$parameters$smooth, " samples smoothing")
    } else {
      smooth_message <- ""
    }
  } else {
    smooth_message <- ""
  }

  if (any(names(x$call_info$parameters) == "thinning")) {
    if (x$call_info$parameters$thinning < 1) {
      thin_message <- paste0(x$call_info$parameters$thinning, "t hinning")
    } else {
      thin_message <- ""
    }
  } else {
    thin_message <- ""
  }


  if (smooth_message == "" & thin_message == "") {
    message2(color = "silver", x = "\n * No smoothing or thinning was applied")
  }

  if (smooth_message != "" & thin_message == "") {
    message2(color = "silver", x = paste("\n *", smooth_message, "was applied"))
  }

  if (smooth_message == "" & thin_message != "") {
    message2(color = "silver", x = paste("\n *", thin_message, "was applied"))
  }
  if (smooth_message != "" & thin_message != "") {
    message2(color = "silver", x = paste("\n *", smooth_message, "and", thin_message, "was applied"))
  }

  # print ohun version
  message2(color = "silver", x = paste0("\n * Created by ", cli::style_bold("ohun "), x$call_info$ohun.version))
}
