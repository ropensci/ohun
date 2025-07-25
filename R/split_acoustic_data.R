#' Splits sound files and associated annotations
#'
#' \code{split_acoustic_data} splits sound files (and corresponding selection tables) in shorter clips
#' @param path Directory path where sound files are found. The current working directory is used as default.
#' @param sgmt.dur Numeric. Duration (in s) of segments in which sound files would be split. Sound files shorter than 'sgmt.dur' won't be split. Ignored if 'sgmts' is supplied.
#' @param sgmts Numeric. Number of segments in which to split each sound file. If supplied 'sgmt.dur' is ignored.
#' @param files Character vector indicating the subset of files that will be split. Supported file formats:'.wav', '.mp3', '.flac' and '.wac'. If not supplied the function will work on all sound files (in the supported format) in 'path'.
#' @param cores Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}. Only used when
#' @param only.sels Logical argument to control if only the data frame is returned (no wave files are saved). Default is \code{FALSE}.
#' @param X 'selection_table' object or a data frame with columns
#' for sound file name (sound.files), selection number (selec), and start and end time of signal
#' (start and end). If supplied the data frame/selection table is modified to reflect the position of the selections in the new sound files. Note that some selections could split between 2 segments. To deal with this, a 'split.sels' column is added to the data frame in which those selection are labeled as 'split'. Default is \code{NULL}.
#' @param output.path Directory path where the output files will be saved. If not supplied then a subfolder called 'clips' will be created within the supplied 'path'.  
#' @family data manipulation
#' @seealso \code{\link[warbleR]{cut_sels}}
#' @export
#' @name split_acoustic_data
#' @return Wave files for each segment (e.g. clips) in the working directory (if \code{only.sels = FALSE}, named as 'sound.file.name-#.wav'). Clips are saved in .wav format. If 'X' is not supplied the function returns a data frame in the containing the name of the original sound files (original.sound.files), the name of the segments (sound.files) and the start and end of segments in the original files. If 'X' is supplied then a data frame with the position of the selections in the newly created clips is returned instead. However, if 'X' is a 'selection table' and the clips have been saved, a data frame with the information of the position of clips in the original sound files is also returned as an attribute in the output selection table ("clip.info"). Output annotation data contains the position of the annotations in the new clips, with an additional column, 'split.sels', that inform users whether annotations have been split into multiple clips ('split') or not (\code{NA}). For split annotations the 'selec' column will contain the original 'selec' id plus an additional index (selec-index) so users can still identify from which annotation splits came from. Sound files in 'path' that are not referenced in 'X' will stil be split. The function may not work properly with very short segments (< 1 s).
#' @details This function aims to reduce the size of sound files in order to simplify some processes that are limited by sound file size (big files can be manipulated, e.g. \code{\link{energy_detector}}).
#' @examples
#' {
#'   # load data and save to temporary working directory
#'   data("lbh1", "lbh2")
#'   tuneR::writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
#'   tuneR::writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
#'
#'   # split files in 1 s files
#'   split_acoustic_data(sgmt.dur = 1, path = tempdir(), files = c("lbh1.wav", "lbh2.wav"))
#'
#'   # Check this folder
#'   tempdir()
#' }
#'
#' @references 
#'  Araya-Salas, M., Smith-Vidaurre, G., Chaverri, G., Brenes, J. C., Chirino, F., Elizondo-Calvo, J., & Rico-Guevara, A. (2023). ohun: An R package for diagnosing and optimizing automatic sound event detection. Methods in Ecology and Evolution, 14, 2259–2271. https://doi.org/10.1111/2041-210X.14170
#' 
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

split_acoustic_data <-
  function(path = ".",
           sgmt.dur = 10,
           sgmts = NULL,
           files = NULL,
           cores = 1,
           pb = TRUE,
           only.sels = FALSE,
           output.path = file.path(path, "clips"),
           X = NULL) {
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
    
    # check path if not provided set to working directory
    path <- if (is.null(path)) {
      getwd()
    } else {
      normalizePath(path)
    }
    
    # create output path if it does not exist
    if (!dir.exists(output.path)) dir.create(output.path, recursive = TRUE)
    
    # measure wav duration
    wvdr <- warbleR::duration_sound_files(path = path, files = files)
    
    # exclude those shorter than sgmt.dur
    if (any(wvdr$duration <= sgmt.dur) & is.null(sgmts)) {
      message2(color = "silver", x = "One or more sound files are shorter than the specified clip duration. No clips will be created for these files.")
      wvdr <- wvdr[wvdr$duration > sgmt.dur, ]
    } 
    
    
    # calculate start and end of clips and output data frame
    split.df_l <- lapply(wvdr$sound.files, function(x) {
      # calculate clip limits
      if (is.null(sgmts)) {
        # if sgmnts duration is shorter than file
        if (sgmt.dur < wvdr$duration[wvdr$sound.files == x]) {
          # get start and end of clips
          sq <-
            seq(
              from = 0,
              to = wvdr$duration[wvdr$sound.files == x],
              by = sgmt.dur
            )
          
          # add end if last sq != duration
          if (sq[length(sq)] != wvdr$duration[wvdr$sound.files == x]) {
            sq <- c(sq, wvdr$duration[wvdr$sound.files == x])
          }
          
          # create data frame with input file information
          out <-
            data.frame(
              original.sound.files = x,
              sound.files = paste0(
                gsub(
                  "\\.wav$|\\.wac$|\\.mp3$|\\.flac$",
                  "",
                  x,
                  ignore.case = TRUE
                ),
                "-",
                1:(length(sq) - 1),
                ".wav"
              ),
              start = sq[-length(sq)],
              end = sq[-1],
              stringsAsFactors = FALSE
            )
        } else {
          # if clip duration is longer or equal
          out <-
            data.frame(
              original.sound.files = x,
              sound.files = x,
              start = 0,
              end = wvdr$duration[wvdr$sound.files == x],
              stringsAsFactors = FALSE
            )
        }
      } else {
        # get start and end of clips
        sq <-
          seq(
            from = 0,
            to = wvdr$duration[wvdr$sound.files == x],
            length.out = sgmts + 1
          )
        
        # put in data frame
        out <-
          data.frame(
            original.sound.files = x,
            sound.files = paste0(
              gsub("\\.wav$|\\.wac$|\\.mp3$|\\.flac$", "", x, ignore.case = TRUE),
              "-",
              1:(length(sq) - 1),
              ".wav"
            ),
            start = sq[-length(sq)],
            end = sq[-1],
            stringsAsFactors = FALSE
          )
      }
      
      return(out)
    })
    
    # put together in a single data frame
    split.df <- do.call(rbind, split.df_l)
    
    # check if there are duplicated file names (witout extension) in the original sound files
    if (any(table(sub("\\.[^.]+$", "", unique(split.df$original.sound.files))) > 1))
      stop2("Some sound files with different formats have the same name (without extension). Please rename them to avoid problems when splitting sound files of use the argument 'files' to focus on a file subset.")
    
    # if clips are produced
    if (!only.sels) {
      # set clusters for windows OS
      if (Sys.info()[1] == "Windows" & cores > 1) {
        cl <-
          parallel::makePSOCKcluster(getOption("cl.cores", cores))
      } else {
        cl <- cores
      }
      
      # split using a loop but only those that are shorter than clips
      a_l <-
        warbleR:::.pblapply(
          pbar = pb,
          X = which(split.df$original.sound.files != split.df$sound.files),
          cl = cl,
          message = "splitting sound files",
          total = 1,
          FUN = function(x) {
            # read clip
            clip <-
              warbleR::read_sound_file(
                X = split.df$original.sound.files[x],
                from = split.df$start[x],
                to = split.df$end[x],
                path = path
              )
            
            # save clip
            tuneR::writeWave(
              extensible = FALSE,
              object = clip,
              filename = file.path(output.path, split.df$sound.files[x])
            )
            
            return(NULL)
          }
        )
    }

    # calculate position of selection in newly created clips
    if (!is.null(X)) {
      split.df2 <- split.df
      
      ## cbind new file data and X to get overlapping sels
      # make analogous columns on both data frames
      split.df2$new.sound.files <- split.df2$sound.files
      split.df2$sound.files <- split.df2$original.sound.files
      split.df2$bottom.freq <- split.df2$top.freq <- NA
      X$new.sound.files <- NA

      # ad unique selec ID to new files
      split.df2$selec <- paste0("new", seq_len(nrow(split.df2)))

       # select columns to bind
      clms <-
        if (!is.null(X$bottom.freq) &
          !is.null(X$top.freq)) {
          c(
            "sound.files",
            "new.sound.files",
            "selec",
            "start",
            "end",
            "bottom.freq",
            "top.freq"
          )
        } else {
          c("sound.files", "new.sound.files", "selec", "start", "end")
        }

      # bind together
      ovlp.df <- rbind(data.frame(X[, clms]), split.df2[, clms])

      # add unique id for each selection
      ovlp.df$sel.id <-
        paste(ovlp.df$sound.files, ovlp.df$selec, sep = "-")
      X$sel.id <- paste(X$sound.files, X$selec, sep = "-")

      # get which selection are found in which new files
      ovlp.df <-
        warbleR::overlapping_sels(
          X = ovlp.df,
          indx.row = TRUE,
          max.ovlp = 0.0000001,
          pb = FALSE,
          parallel = 1,
          verbose = FALSE
        )

      ovlp.df$..row <- seq_len(nrow(ovlp.df))

      # split in new files rows and selection rows
      new.sf.df <-
        ovlp.df[!is.na(ovlp.df$new.sound.files) &
          !is.na(ovlp.df$indx.row), ]
      org.sls.df <- ovlp.df[is.na(ovlp.df$new.sound.files), ]
      org.sls.df$sel.id <-
        paste(org.sls.df$sound.files, org.sls.df$selec, sep = "-")
      # re-add other columns
      X$original.sound.files <- X$sound.files
      org.sls.df <-
        merge(org.sls.df, X[, setdiff(names(X), clms)], by = "sel.id")

      # order columns
      org.sls.df <- warbleR::sort_colms(org.sls.df)

      # find time positions in new files
      new.sels_l <- lapply(seq_len(nrow(new.sf.df)), function(x) {
        Y <- new.sf.df[x, , drop = FALSE]

        # get those selection found within Y
        contained.sls <-
          org.sls.df[org.sls.df$..row %in% strsplit(Y$indx.row, "/")[[1]], ]

        # if selection were found within Y
        if (nrow(contained.sls) > 0) {
          contained.sls$sound.files <- Y$new.sound.files

          # get new start and end
          contained.sls$start <- contained.sls$start - Y$start
          contained.sls$end <- contained.sls$end - Y$start
          contained.sls$start[contained.sls$start < 0] <- 0
          
          # fix end if longer than clip duration 
          wav_duration <- split.df2$end[split.df2$new.sound.files == new.sf.df$new.sound.files[x][1]] - split.df2$start[split.df2$new.sound.files == new.sf.df$new.sound.files[x][1]]
          contained.sls$end[contained.sls$end > wav_duration] <- (wav_duration - 0.0001) # to avoid errors when creating selection tables

          return(contained.sls)
        } else {
          return(NULL)
        }
      })

      # put together clip information in a single data frame
      new.sels <- do.call(rbind, new.sels_l)
      new.sels$..row <-
        new.sels$indx.row <-
        new.sels$ovlp.sels <- new.sels$new.sound.files <- NULL
      
      # find which selection were split in 2 or more new files
      new.sels$split.sels <- NA
      cnt.sels <- table(new.sels$sel.id)
      new.sels$split.sels[new.sels$sel.id %in% names(cnt.sels[cnt.sels > 1])] <-
        "split"
    
      row.names(new.sels) <- seq_len(nrow(new.sels))

      # warn users that some annotations were split and relabel selec IDs
      if (any(!is.na(new.sels$split.sels))){
        
        for (e in names(cnt.sels[cnt.sels > 1])) {
          
          new.sels$selec[new.sels$sel.id == e] <- paste(new.sels$selec[new.sels$sel.id == e], seq_len(cnt.sels[e]), sep = "-")
        }
        message2(color = "silver", x = "Some annotations were split into multiple clips (check column 'split.sels' in the output data)")
      }
      
      new.sels$sel.id <- NULL
      
      # convert back to selection table
      if (warbleR::is_selection_table(X) & !only.sels) {
        
        new.sels <- warbleR::selection_table(X = new.sels, path = output.path, parallel = cores, pb = FALSE, verbose = FALSE)

        attributes(new.sels)$call <- base::match.call()

        attributes(new.sels)$elapsed.time.s <- as.vector((proc.time() - start_time)[3])
      
        attributes(new.sels)$clip.info <- split.df
        }

      
      return(new.sels)
    } else {
      return(split.df)
    }
  }
