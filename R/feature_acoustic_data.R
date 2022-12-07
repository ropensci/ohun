#' @title Summarize information about file format in an acoustic data set
#'
#' @description \code{feature_acoustic_data} summarizes information about file format in an acoustic data set
#' @usage feature_acoustic_data(path = ".", digits = 2)
#' @param path Character string containing the directory path where the sound files are located. Default is \code{"."} (current working directory).
#' @param digits Numeric vector of length 1 with the number of decimals to include. Default is 2.
#' @return The function prints a summary of the format of the files in an acoustic data set.
#' @export
#' @name feature_acoustic_data
#' @details The function summarizes information about file format in an acoustic data set. It provides information about the number of files, file formats, sampling rates, bit depts, channels, duration and file size (in MB). For file format, sampling rate, bit depth and number of channels the function includes information about the number of files for each format (e.g. '44.1 kHz (2)' means 2 files with a sampling rate of 44.1 kHz).
#' @examples {
#' # load data and save example files into temporary working directory
#' data("lbh1", "lbh2", "lbh_reference")
#' writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
#' writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
#'
#' # summary across sound files
#' feature_acoustic_data(path = tempdir())
#' }
#' @seealso \code{\link{feature_reference}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references {
#' Araya-Salas, M. (2021), ohun: diagnosing and optimizing automated sound event detection. R package version 0.1.0.
#' }
# last modification on jul-2022 (MAS)

feature_acoustic_data <- function(path = ".", digits = 2) {
  # get information by sound file
  info_files <- warbleR::info_sound_files(path = path, pb = FALSE)

  # get file size (should be removed after updating warbleR to )
  info_files$wav.size <-
    file.size(file.path(path, info_files$sound.files)) / 1000000

  cat(crayon::silver(paste0(
    "Features of the acoustic data set in '", crayon::bold(paste0(normalizePath(path), "':"))
  )))

  # number of files
  cat(crayon::silver(paste("\n*", nrow(info_files), "sound files")))

  # formats
  pos <- regexpr("\\.([[:alnum:]]+)$", info_files$sound.files)
  extensions <-
    ifelse(pos > -1L, substring(info_files$sound.files, pos + 1L), "")
  extensions <- tolower(extensions)
  extensions <- paste0(".", extensions)
  tab_ext <- table(extensions)
  tab_ext <- paste0(names(tab_ext), " (", tab_ext, ")")
  tab_ext <- paste(tab_ext, collapse =  "; ")

  format_message <-
    paste0("\n* ", length(unique(extensions)), " file format(s) ", "(", tab_ext, ")")
  if (length(unique(extensions)) > 1)
    cat(crayon::red(format_message)) else
    cat(crayon::silver(format_message))

  # sampling rates
  tab_sr <- table(info_files$sample.rate)
  tab_sr <- paste0(names(tab_sr), " kHz", " (", tab_sr, ")")
  tab_sr <- paste(tab_sr, collapse =  "; ")

  sr_message <-
    paste0("\n* ", length(unique(info_files$sample.rate)), " sampling rate(s) ", "(", tab_sr, ")")
  if (length(unique(info_files$sample.rate)) > 1)
    cat(crayon::red(sr_message)) else
    cat(crayon::silver(sr_message))

  # bit depths
  tab_bd <- table(info_files$bits)
  tab_bd <- paste0(names(tab_bd), " bits", " (", tab_bd, ")")
  tab_bd <- paste(tab_bd, collapse =  "; ")

  bd_message <-
    paste0("\n* ", length(unique(info_files$bits)), " bit depth(s) ", "(", tab_bd, ")")
  if (length(unique(info_files$bits)) > 1)
    cat(crayon::red(bd_message)) else
    cat(crayon::silver(bd_message))

  # number of channels
  tab_ch <- table(info_files$channels)
  tab_ch <- paste0(names(tab_ch), " channel(s)", " (", tab_ch, ")")
  tab_ch <- paste(tab_ch, collapse =  "; ")

  ch_message <-
    paste0("\n* ", length(unique(info_files$channels)), " number of channels ", "(", tab_ch, ")")
  if (length(unique(info_files$channels)) > 1)
    cat(crayon::red(ch_message)) else
    cat(crayon::silver(ch_message))

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

  cat(crayon::silver(dur_message))

  # file size
  tab_sz <- table(info_files$channels)
  tab_sz <- paste0(names(tab_sz), " MB", " (", tab_sz, ")")
  tab_sz <- paste(tab_sz, collapse =  "; ")
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

  cat(crayon::silver(dur_message))

  cat(crayon::italic(
    crayon::silver(
      "\n (detailed information by sound file can be obtained with 'warbleR::info_sound_files()')"
    )
  ))
}
