#' @title Summarize information about file format in an acoustic data set
#'
#' @description \code{summarize_acoustic_data} summarizes information about file format in an acoustic data set
#' @usage summarize_acoustic_data(path = ".", digits = 2)
#' @param path Character string containing the directory path where the sound files are located. Default is \code{"."} (current working directory).
#' @param digits Numeric vector of length 1 with the number of decimals to include. Default is 2.
#' @return The function prints a summary of the format of the files in an acoustic data set.
#' @export
#' @name summarize_acoustic_data
#' @details The function summarizes information about file format in an acoustic data set. It provides information about the number of files, file formats, sampling rates, bit depts, channels, duration and file size (in MB). For file format, sampling rate, bit depth and number of channels the function includes information about the number of files for each format (e.g. '44.1 kHz (2)' means 2 files with a sampling rate of 44.1 kHz).
#' @examples {
#'   # load data and save example files into temporary working directory
#'   data("lbh1", "lbh2", "lbh_reference")
#'   tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
#'   tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
#'
#'   # summary across sound files
#'   summarize_acoustic_data(path = tempdir())
#' }
#' @seealso \code{\link{summarize_reference}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references 
#' Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C., Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. 2022. ohun: an R package for diagnosing and optimizing automatic sound event detection. BioRxiv, 2022.12.13.520253. https://doi.org/10.1101/2022.12.13.520253
#' 

summarize_acoustic_data <- function(path = ".", digits = 2) {
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

  # get information by sound file
  info_files <- warbleR::info_sound_files(path = path, pb = FALSE)

  # get file size (should be removed after updating warbleR to )
  info_files$wav.size <-
    file.size(file.path(path, info_files$sound.files)) / 1000000

  message2(color = "silver", x = paste0(
    "Features of the acoustic data set in '", cli::style_bold(paste0(normalizePath(path), "':"))
  ))

  # number of files
  message2(color = "silver", x = paste("\n*", nrow(info_files), "sound files"))

  # formats
  pos <- regexpr("\\.([[:alnum:]]+)$", info_files$sound.files)
  extensions <-
    ifelse(pos > -1L, substring(info_files$sound.files, pos + 1L), "")
  extensions <- tolower(extensions)
  extensions <- paste0(".", extensions)
  tab_ext <- table(extensions)
  tab_ext <- paste0(names(tab_ext), " (", tab_ext, ")")
  tab_ext <- paste(tab_ext, collapse = "; ")

  format_message <-
    paste0("\n* ", length(unique(extensions)), " file format(s) ", "(", tab_ext, ")")
  if (length(unique(extensions)) > 1) {
    message2(color = "red", x = format_message)
  } else {
    message2(color = "silver", x = format_message)
  }

  # sampling rates
  tab_sr <- table(info_files$sample.rate)
  tab_sr <- paste0(names(tab_sr), " kHz", " (", tab_sr, ")")
  tab_sr <- paste(tab_sr, collapse = "; ")

  sr_message <-
    paste0("\n* ", length(unique(info_files$sample.rate)), " sampling rate(s) ", "(", tab_sr, ")")
  if (length(unique(info_files$sample.rate)) > 1) {
    message2(color = "red", x = sr_message)
  } else {
    message2(color = "silver", x = sr_message)
  }

  # bit depths
  tab_bd <- table(info_files$bits)
  tab_bd <- paste0(names(tab_bd), " bits", " (", tab_bd, ")")
  tab_bd <- paste(tab_bd, collapse = "; ")

  bd_message <-
    paste0("\n* ", length(unique(info_files$bits)), " bit depth(s) ", "(", tab_bd, ")")
  if (length(unique(info_files$bits)) > 1) {
    message2(color = "red", x = bd_message)
  } else {
    message2(color = "silver", x = bd_message)
  }

  # number of channels
  tab_ch <- table(info_files$channels)
  tab_ch <- paste0(names(tab_ch), " channel(s)", " (", tab_ch, ")")
  tab_ch <- paste(tab_ch, collapse = "; ")

  ch_message <-
    paste0("\n* ", length(unique(info_files$channels)), " number of channels ", "(", tab_ch, ")")
  if (length(unique(info_files$channels)) > 1) {
    message2(color = "red", x = ch_message)
  } else {
    message2(color = "silver", x = ch_message)
  }

  # duration
  dur_message <-
    paste0(
      "\n* File duration range: ",
      round(min(info_files$duration), digits),
      "-",
      round(max(info_files$duration), digits),
      " s (mean: ",
      round(mean(info_files$duration), digits),
      " s)"
    )

  message2(color = "silver", x = dur_message)

  # file size
  tab_sz <- table(info_files$channels)
  tab_sz <- paste0(names(tab_sz), " MB", " (", tab_sz, ")")
  tab_sz <- paste(tab_sz, collapse = "; ")
  dur_message <-
    paste0(
      "\n* File size range: ",
      round(min(info_files$wav.size), digits),
      "-",
      round(max(info_files$wav.size), digits),
      " MB (mean: ",
      round(mean(info_files$wav.size), digits),
      " MB)"
    )

  message2(color = "silver", x = dur_message)

  message2(color = "silver", cli::style_italic(
    "\n (detailed information by sound file can be obtained with 'warbleR::info_sound_files()')"
  ))
}



##############################################################################################################
#' alternative name for \code{\link{summarize_acoustic_data}}
#'
#' @keywords internal
#' @details see \code{\link{summarize_acoustic_data}} for documentation. \code{\link{feature_acoustic_data}} will be deprecated in future versions.
#' @export

feature_acoustic_data <- summarize_acoustic_data
