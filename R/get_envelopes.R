#' @title  Extract absolute amplitude envelopes
#'
#' @description \code{get_envelopes} extracts absolute amplitude envelopes to speed up energy detection
#' @usage get_envelopes(path = ".", files = NULL, bp = NULL, hop.size = 11.6, wl = NULL,
#' cores = 1, thinning = 1, pb = TRUE, smooth = 5, normalize = TRUE)
#' @param path Character string containing the directory path where the sound files are located.
#'The current working directory is used as default.
#' @param files character vector or indicating the sound files that will be analyzed.
#' @param bp Numeric vector of length 2 giving the lower and upper limits of a
#'   frequency bandpass filter (in kHz). Default is \code{NULL}.
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
#'   with a sum smooth function. It controls the time range (in ms) in which amplitude samples are smoothed (i.e. averaged with neighboring samples). Default is 5. 0 means no smoothing is applied. Note that smoothing is applied before thinning (see 'thinning' argument).
#' @param normalize Logical argument to control if envelopes are normalized to a 0-1 range.
#' @return An object of class 'envelopes'.
#' @export
#' @name get_envelopes
#' @details This function extracts the absolute amplitude envelopes of sound files. Can be used to manipulate envelopes before running \code{\link{energy_detector}}.
#'
#' @examples {
#' # Save to temporary working directory
#' data(list = c("lbh1", "lbh2"))
#' writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
#' writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
#'
#' # get raw absolute amplitude envelopes
#' envs <- get_envelopes(path = tempdir())
#'
#' # extract segment for the first sound event in the first sound file
#' x <- envs[[1]]$envelope
#'
#' # and plot it
#' plot(x[(length(x)/9):(length(x)/4)], type = "l", xlab = "samples", ylab = "amplitude")
#'
#' # smoothing envelopes
#' envs <- get_envelopes(path = tempdir(), smooth = 6.8)
#' x <- envs[[1]]$envelope
#' plot(x[(length(x)/9):(length(x)/4)], type = "l", xlab = "samples", ylab = "amplitude")
#'
#' # smoothing and thinning
#' envs <- get_envelopes(path = tempdir(), thinning = 1/10, smooth = 6.8)
#' x <- envs[[1]]$envelope
#' plot(x[(length(x)/9):(length(x)/4)], type = "l", xlab = "samples", ylab = "amplitude")
#'
#' # no normalization
#' envs <- get_envelopes(path = tempdir(), thinning = 1/10, smooth = 6.8)
#' x <- envs[[1]]$envelope
#' plot(x[(length(x)/9):(length(x)/4)], type = "l", xlab = "samples", ylab = "amplitude",
#' normalize = FALSE)
#' }
#'
#' @references {
#' Araya-Salas, M. (2021), ohun: diagnosing and optimizing automated sound event detection. R package version 0.1.0.
#' }
#' @seealso \code{\link{energy_detector}}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}).
#last modification on aug-31-2021 (MAS)

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
           normalize = TRUE
           ) {

    #check path if not provided set to working directory
    if (is.null(path))
      path <- getwd() else
      if (!dir.exists(path))
        stop2("'path' provided does not exist") else
      path <- normalizePath(path)

        # hopsize
        if (!is.numeric(hop.size) | hop.size < 0) stop2("'hop.size' must be a positive number")

    #if bp is not vector or length!=2 stop
    if (!is.null(bp))
    {
      if (!is.vector(bp))
        stop2("'bp' must be a numeric vector of length 2")  else {
        if (!length(bp) == 2)
          stop2("'bp' must be a numeric vector of length 2")
      }
    }

    #if smooth is not vector or length!=1 stop
      if (!is.vector(smooth))
        stop2("'smooth' must be a numeric vector of length 1") else {
        if (!length(smooth) == 1)
          stop2("'smooth' must be a numeric vector of length 1")
      }

    #if thinning is not vector or length!=1 between 1 and 0
    if (!is.vector(thinning) | !is.numeric(thinning))
      stop2("'thinning' must be a numeric vector of length 1")
        if (thinning[1] > 1 | thinning[1] <= 0)
          stop2("'thinning' must be greater than 0 and lower than or equal to 1")

    #if wl is not vector or length!=1 stop
    if (!is.null(wl)) {
      if (!is.vector(wl))
        stop2("'wl' must be a numeric vector of length 1") else {
        if (!length(wl) == 1)
          stop2("'wl' must be a numeric vector of length 1")
      }
    }

    #if files is not character vector
    if (!is.null(files) &
        any(!is.character(files),!is.vector(files)))
      stop2("'files' must be a character vector")

    #if cores is not numeric
    if (!is.numeric(cores))
      stop2("'cores' must be a numeric vector of length 1")
    if (any(!(cores %% 1 == 0), cores < 1))
      stop2("'cores' should be a positive integer")

    #return error if not all sound files were found
    if (is.null(files))
      files <- list.files(path = path,
                 pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$",
                 ignore.case = TRUE) else
                   if (!all(files %in% list.files(path = path,
                                                  pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$",
                                                  ignore.case = TRUE)))
                     stop2("Some (or all) sound files were not found")

    if (length(files) == 0)
      stop2("no sound files found in working directory or 'path' supplied")

    #Apply over each sound file
    # set clusters for windows OS
    if (Sys.info()[1] == "Windows" & cores > 1)
      cl <- parallel::makeCluster(cores) else
      cl <- cores

    # run function over sound files or selections in loop
    env_list <- warbleR:::pblapply_wrblr_int(pbar = pb,
      X = files,
      cl = cl,
      FUN = function(i)
      {
        env_ohun_int(i,
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
    env_list[[length(env_list) + 1]]  <- list(
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

  cat(crayon::black(paste("Object of class", crayon::bold("'envelopes' \n"))))

  cat(crayon::silver(paste("* The output of the following", crayon::italic("get_envelopes()"), "call: \n")))

  cll <- paste0(deparse(x$call_info$call))
  cat(crayon::silver(crayon::italic(gsub("    ", "", cll), "\n")))

  file_names <- names(x)[-length(x)]

  cat(crayon::silver(paste("* Contains the amplitude envelopes of"), length(x) - 1, "sound file(s):\n", paste(crayon::italic(utils::head(file_names), collapse = " ")), if( length(file_names) > 6) paste("... and", length(file_names) - 6, "more") else ""))

  # add message about amplitude envelope modifications
  if (any(names(x$call_info$parameters) == "smooth")){
    if (x$call_info$parameters$smooth > 0)
      smooth_message <- paste0(x$call_info$parameters$smooth, " samples smoothing") else smooth_message <- ""
      } else
        smooth_message <- ""

  if (any(names(x$call_info$parameters) == "thinning")){
    if (x$call_info$parameters$thinning < 1)
      thin_message <- paste0(x$call_info$parameters$thinning, "t hinning") else thin_message <- ""
  } else
    thin_message <- ""


  if (smooth_message == "" & thin_message == "") cat(crayon::silver("\n * No smoothing or thinning was applied"))

  if (smooth_message != "" & thin_message == "")
    cat(crayon::silver(paste("\n *", smooth_message, "was applied")))

  if (smooth_message == "" & thin_message != "")
    cat(crayon::silver(paste("\n *", thin_message, "was applied")))
  if (smooth_message != "" & thin_message != "")
    cat(crayon::silver(paste("\n *", smooth_message, "and", thin_message, "was applied")))

  # print ohun version
    cat(crayon::silver(paste0("\n * Created by ", crayon::bold("ohun "), x$call_info$ohun.version)))
}

########################### internal function to get an envelope ###################

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
           normalize
  )
  {

    # read wave object
    wave_obj <- warbleR::read_sound_file(X = i, path = path)

    # adjust wl based on hope.size (convert to samples)
    if (is.null(wl))
      wl <- round(wave_obj@samp.rate * hop.size  / 1000, 0)

    # make wl even if odd
    if (!(wl %% 2) == 0) wl <- wl + 1

    # convert smooth to samples
    smooth <- round(wave_obj@samp.rate * smooth  / 1000, 0)

    #filter frequencies
    if (!is.null(bp))
    {
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

    #detect sound events based on amplitude (modified from seewave::timer function)
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
    if (normalize)
    {
      envp <- envp - min(envp)
      envp <- envp / max(envp)
    }

    # thin
    if (thinning < 1) {

      if (n.samples * thinning < 10)  stop2("thinning is too high, no enough samples left for at least 1 sound file")

      # reduce size of envelope
      envp <-
        stats::approx(
          x = seq(0, wave_dur, length.out = length(envp)),
          y = envp,
          n = round(n.samples * thinning),
          method = "linear"
        )$y
    }

    output <- list(envelope = envp,
                   duration = wave_dur,
                   org.n.samples = n.samples,
                   sampling.freq = wave_obj@samp.rate
    )

    return(output)
  }
