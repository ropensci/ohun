#' @title Reassemble annotations from clips
#'
#' @description \code{reassemble_detection} reassembles detections made on clips so they refer to the original sound files
#' @param detection Data frame or selection table (using the warbleR package's format, see \code{\link[warbleR]{selection_table}}) containing the start and end of the signals. Must contained at least the following columns: "sound.files", "selec", "start" and "end". 
#' @param Y Data frame with the start and end of clips in the orignal sound files. Must contain the column "original.sound.files", "sound.files" (clip files), "start" and "end".
#' @param cores Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return A data frame or selection table (if 'detection' was also a selection table, warbleR package's format, see \code{\link[warbleR]{selection_table}}) as in 'detection' but removing ambiguous detections (split and merged positives).
#' @export
#' @name reassemble_detection
#' @details This function will take detections made on clips created with \code{\link{split_acoustic_data}}, and reset their information so they refer back to the original (unsplit) sound files.
#'
#' @examples {
#' # load example data
#' data("lbh1", "lbh2", "lbh_reference")
#' tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
#' tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
#'  
#' ## if X is a data frame #####
#' df_ref <- as.data.frame(lbh_reference)
#'   
#' # get split annotations
#' split_df_ref <- split_acoustic_data(X = df_ref,
#'  only.sels = TRUE, sgmt.dur = 1.5, 
#'  path = tempdir(), pb = FALSE, 
#'  files = c("lbh1.wav", "lbh2.wav"))
#'    
#' # get clip information
#' Y <- split_acoustic_data(sgmt.dur = 1.5, 
#'  path = tempdir(), pb = FALSE,
#'  output.path = tempdir(), files = c("lbh1.wav", "lbh2.wav"))
#'    
#' # reassemble annotations
#' tc <- reassemble_detection(detection = split_df_ref,  
#' Y = Y, pb = FALSE)
#'
#' # start and end are the same as in the original unsplit data
#' df_ref <- df_ref[order(df_ref$sound.files, df_ref$start), ]
#' all(tc$end == df_ref$end)
#' all(tc$start == df_ref$start)
#'  
#' ### if X is a selection table ##
#' # split annotations and files
#' split_lbh_reference <- split_acoustic_data(X = lbh_reference, 
#'  sgmt.dur = 1.5, path = tempdir(),
#'  output.path = tempdir(),
#'  files = c("lbh1.wav", "lbh2.wav"))
#' 
#' # reassemble annotations
#' tc <- reassemble_detection(detection = split_lbh_reference, 
#'   Y = attributes(split_lbh_reference)$clip.info)
#'   
#' # start and end are the same as in the original unsplit data
#' lbh_reference <- lbh_reference[order(lbh_reference$sound.files, lbh_reference$start), ]
#' all(tc$end == lbh_reference$end)
#' all(tc$start == lbh_reference$start)
#' }
#'
#' @references
#' Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C., Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. (2023). ohun: An R package for diagnosing and optimizing automatic sound event detection. Methods in Ecology and Evolution, 14, 2259–2271. https://doi.org/10.1111/2041-210X.14170
#'
#' @seealso \code{\link{label_detection}}, \code{\link{split_acoustic_data}}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}).

reassemble_detection <- function(detection,
                                 Y,
                                 cores = 1,
                                 pb = TRUE)
{
  # save start time
  start_time <- proc.time()
  
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
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & cores > 1) {
    cl <- parallel::makeCluster(cores)
  } else {
    cl <- cores
  }
  
  anns_list <- warbleR:::.pblapply(
    X = unique(detection$sound.files),
    cl = cl,
    message = "reassembling detections",
    total = 1,
    pbar = pb,
    FUN = function(x) {
    
    # Filter detection for the current sound file
    W <- as.data.frame(detection[detection$sound.files == x, ])
    
    # rename columns
    W$clip.start <- W$start
    W$clip.end <- W$end
    W$clip.selec <- W$selec
    W$clip <- W$sound.files
    
    # adjust the start and end times based on Y
    W$sound.files <- Y$original.sound.files[Y$sound.files == x]
    W$start <- W$clip.start + Y$start[Y$sound.files == x]
    W$end <- W$clip.end + Y$start[Y$sound.files == x]
    W$selec <- seq_len(nrow(W))
    
    return(W)
})
  
  # Combine the list of data frames into a single data frame
  reassemble_anns <- do.call(rbind, anns_list)
  reassemble_anns <- reassemble_anns[order(reassemble_anns$sound.files, reassemble_anns$start), ]
  
  return(reassemble_anns)
  }