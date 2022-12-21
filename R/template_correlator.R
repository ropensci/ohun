#' Acoustic templates correlator using time-frequency cross-correlation
#'
#' \code{template_correlator} estimates templates cross-correlation across multiple sound files.
#' @usage template_correlator(templates, files = NULL, hop.size = 11.6, wl = NULL, ovlp = 0,
#' wn ='hanning', cor.method = "pearson", cores = 1, path = ".",
#' pb = TRUE, type = "fourier", fbtype = "mel", ...)
#' @param templates 'selection_table', 'extended_selection_table' (warbleR package's formats, see \code{\link[warbleR]{selection_table}}) or data frame with time and frequency information of the sound event(s) to be used as templates (1 template per row). The object must containing columns for sound files (sound.files),
#' selection number (selec), and start and end time of sound event (start and end). If frequency range columns are included ('bottom.freq' and 'top.freq', in kHz) the correlation will be run on those frequency ranges. All templates must have the same sampling rate and both templates and 'files' (in which to find templates) must also have the same sampling rate.
#' @param hop.size A numeric vector of length 1 specifying the time window duration (in ms). Default is 11.6 ms, which is equivalent to 512 wl for a 44.1 kHz sampling rate. Ignored if 'wl' is supplied.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram. Default is \code{NULL}. If supplied, 'hop.size' is ignored.
#' @param ovlp Numeric vector of length 1 specifying \% of overlap between two
#' consecutive windows, as in \code{\link[seewave]{spectro}}. Default is 0. High values of ovlp
#' slow down the function but may produce more accurate results.
#' @param wn A character vector of length 1 specifying the window name as in \code{\link[seewave]{ftwindow}}.
#' @param cor.method A character vector of length 1 specifying the correlation method as in \code{\link[stats]{cor}}.
#' @param cores Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param path Character string containing the directory path where the sound files are located.
#'The current working directory is used as default.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param files Character vector with the selections in 'X' to be used as surveys for cross-correlation detection. To refer to specific selections in 'X' the user must use the format "sound.file-selec" (e.g. "file1.wav-1"). If only the sound file name is included then the entire sound file is used as survey.
#' @param type A character vector of length 1 specifying the type of cross-correlation: "fourier" (i.e. spectrographic cross-correlation using Fourier transform; internally using \code{\link[seewave]{spectro}}; default), "mfcc" (auditory scale coefficient matrix cross-correlation; internally using \code{\link[tuneR]{melfcc}}) or "auditory-spectrum" (cross-correlation of auditory spectrum, i.e. spectrum after transformation to an auditory scale; internally using \code{\link[tuneR]{melfcc}}). The argument 'fbtype' controls the auditory scale to be used. Note that the last 2 methods have not been widely used in this context so can be regarded as experimental.
#' @param fbtype Character vector indicating the auditory frequency scale to use: "mel", "bark", "htkmel", "fcmel".
#' @param ... Additional arguments to be passed to \code{\link[tuneR]{melfcc}} for further customization when using auditory scales.
#' @return The function returns an object of class 'template_correlations' which is a list with the correlation scores for each combination of templates and files. 'template_correlations' objects must be used to infer sound event ocurrences using \code{\link{template_detector}} or to graphically explore template correlations across sound files using \code{\link[warbleR]{full_spectrograms}}.
#'
#' @export
#' @name template_correlator
#' @details This function calculates the similarity of acoustic templates across sound files by means of time-frequency cross-correlation. Fourier spectrograms or time-frequency representations from auditory scales (including ceptral coefficients) can be used. Several templates can be run over several sound files. Note that template-based detection is divided in two steps: template correlation (using this function) and template detection (or peak detection as it infers detection based on peak correlation scores, using the function \code{\link{template_detector}}). So the output of this function (and object of 'template_correlations') must be input into \code{\link{template_detector}} for inferring sound event occurrences. \code{\link{optimize_template_detector}} can be used to optimize template detection.
#' @examples
#' {
#' #load example data
#' data("lbh1", "lbh2", "lbh_reference")
#'
#' #save sound files
#' writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
#' writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
#'
#' # create template
#' templ <- lbh_reference[4,]
#' templ2 <- selection_table(templ, extended = TRUE, confirm.extended = FALSE,
#' path = tempdir())
#'
#' # fourier spectrogram
#' (tc_fr <- template_correlator(templates = templ, path = tempdir(), type = "fourier"))
#'
#' # mel auditory spectrograms
#' (tc_ma <- template_correlator(templates = templ, path = tempdir(), type = "mel-auditory"))
#'
#' # mfcc spectrograms
#' (tc_mfcc <- template_correlator(templates = templ, path = tempdir(), type = "mfcc"))
#'
#' # similar results (but no exactly the same) are found with the 3 methods
#' # these are the correlation of the correlation vectors
#' # fourier vs mel-auditory
#' cor(tc_fr$`lbh2.wav-4/lbh2.wav`$correlation.scores,
#' tc_ma$`lbh2.wav-4/lbh2.wav`$correlation.scores)
#'
#' # fourier vs mfcc
#' cor(tc_fr$`lbh2.wav-4/lbh2.wav`$correlation.scores,
#' tc_mfcc$`lbh2.wav-4/lbh2.wav`$correlation.scores)
#'
#' # mel-auditory vs mfcc
#' cor(tc_ma$`lbh2.wav-4/lbh2.wav`$correlation.scores,
#' tc_mfcc$`lbh2.wav-4/lbh2.wav`$correlation.scores)
#'
#' # using an extended selection table
#' templ_est <- selection_table(templ, extended = TRUE, confirm.extended = FALSE, path = tempdir())
#'
#' tc_fr_est <- template_correlator(templates = templ_est, path = tempdir(), type = "fourier")
#'
#' # produces the same result as templates in a regular data frame
#' cor(tc_fr$`lbh2.wav-4/lbh2.wav`$correlation.scores,
#' tc_fr_est$`lbh2.wav_4-1/lbh2.wav`$correlation.scores)
#' }
#' @seealso \code{\link{energy_detector}}, \code{\link{template_detector}}, \code{\link{optimize_template_detector}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references {
#' Araya-Salas, M. (2021), ohun: diagnosing and optimizing automated sound event detection. R package version 0.1.0.
#'
#' Khanna H., Gaunt S.L.L.  & McCallum D.A. (1997). Digital spectrographic cross-correlation: tests of recall. Bioacoustics 7(3): 209-234.
#'
#' Lyon, R. H., & Ordubadi, A. (1982). Use of cepstra in acoustical signal analysis. Journal of Mechanical Design, 104(2), 303-306.
#' }
# last modification on aug-25-2021 (MAS)

template_correlator <-
  function(templates,
           files = NULL,
           hop.size = 11.6,
           wl = NULL,
           ovlp = 0,
           wn = 'hanning',
           cor.method = "pearson",
           cores = 1,
           path = ".",
           pb = TRUE,
           type = "fourier",
           fbtype = "mel",
           ...)
  {
    #check path to working directory
    if (is.null(path))
      path <- getwd() else
      if (!dir.exists(path))
        stop2("'path' provided does not exist") else
      path <- normalizePath(path)

    # hopsize
    if (!is.numeric(hop.size) |
        hop.size < 0)
      stop2("'hop.size' must be a positive number")

    #if there are NAs in start or end stop
    if (any(is.na(c(templates$end, templates$start))))
      stop2("NAs found in start and/or end")

    #if wl is not vector or length!=1 stop
    if (!is.null(wl)) {
      if (!is.numeric(wl))
        stop2("'wl' must be a numeric vector of length 1") else {
        if (!is.vector(wl))
          stop2("'wl' must be a numeric vector of length 1") else {
          if (!length(wl) == 1)
            stop2("'wl' must be a numeric vector of length 1")
        }
      }
    }

    #if ovlp is not vector or length!=1 stop
    if (!is.numeric(ovlp))
      stop2("'ovlp' must be a numeric vector of length 1") else {
      if (!is.vector(ovlp))
        stop2("'ovlp' must be a numeric vector of length 1") else {
        if (!length(ovlp) == 1)
          stop2("'ovlp' must be a numeric vector of length 1")
      }
    }

    # check files or list files in working directory
    if (!is.null(files)) {
      if (any(!is.character(files), !is.vector(files)))
        stop2("'files' must be a character vector")

      # check files are in working directory
      if (!any(
        files %in% list.files(
          path = path,
          pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$",
          ignore.case = TRUE
        )
      ))
        stop2("At least one sound files in 'files' was not found")
    } else
      files <-
        list.files(path = path,
                   pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$",
                   ignore.case = TRUE)

    # check if files in templates are in path
    if (!is_extended_selection_table(templates))
      if (!any(
        templates$sound.files %in% list.files(
          path = path,
          pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$",
          ignore.case = TRUE
        )
      ))
        stop2(
          "At least one sound files in 'templates' was not found in the working directory or 'path' supplied"
        )

    # If cores is not numeric
    if (!is.numeric(cores))
      stop2("'cores' must be a numeric vector of length 1")
    if (any(!(cores %% 1 == 0), cores < 1))
      stop2("'cores' should be a positive integer")

    # get sampling rate of files
    info_sf <- warbleR::info_sound_files(path = path, pb = FALSE)

    # get subset of files in 'files' and 'templates'
    info_sf <-
      info_sf[info_sf$sound.files %in% c(files, if (!is_extended_selection_table(templates))
        templates$sound.files else
          NULL),]

    # check if sampling rate in files and template is the same
    if (length(unique(info_sf$sample.rate)) > 1)
      stop2("sampling rate must be the same for all templates and sound files")

    # check sampling rate is the same for all templates if not a selection table
    if (is_extended_selection_table(templates) &
        length(unique(attr(templates, "check.results")$sample.rate)) > 1)
      stop2("sampling rate must be the same for all templates and sound files")

    # check sampling rate is the same for templates and sound files if not a selection table
    if (is_extended_selection_table(templates))
      if (unique(attr(templates, "check.results")$sample.rate) != unique(info_sf$sample.rate))

        # set bottom and top freq if not supplied
        if (is.null(templates$bottom.freq)) {
          templates$top.freq <- unique(info_sf$sample.rate) / 2
          templates$bottom.freq <- 0
        }

    # add selection id column to templates
    templates$selection.id <-
      paste(templates$sound.files, templates$selec, sep = "-")

    # create compare.matrix
    compare.matrix <-
      expand.grid(templates = templates$selection.id, files = files)


    # function to get spectrogram matrices
    spc_FUN <-
      function(j,
               pth,
               W,
               hop,
               wlg,
               ovl,
               w,
               bndpss,
               nbnds,
               entire = FALSE,
               fbt,
               ...) {
        # read entire sound file
        if (entire)
          clp <- warbleR::read_sound_file(X = j, path = pth) else
          clp <- warbleR::read_sound_file(X = W,
                                          index = j,
                                          path = pth)

        # adjust wl based on hope.size
        if (is.null(wlg))
          wlg <- round(clp@samp.rate * hop / 1000, 0)

        # make wl even if odd
        if (!(wlg %% 2) == 0)
          wlg <- wlg + 1

        # steps for time bins
        steps <-  seq(1, length(clp@left) - wlg, wlg - (ovlp * wlg / 100))

        if (type == "fourier") {
          spc <-
            warbleR:::stft_wrblr_int(
              wave = matrix(clp@left),
              f = clp@samp.rate,
              wl = wlg,
              zp = 0,
              step = steps,
              wn = w,
              fftw = FALSE,
              scale = TRUE,
              complex = FALSE
            )

          # calculate freq values for spc rows
          freq <-
            seq(0,
                (clp@samp.rate / 2) - (clp@samp.rate / wlg),
                length.out = nrow(spc)) / 1000

          # apply bandpass
          spc <- spc[freq >= bndpss[1] & freq <= bndpss[2],]

          # log amplitude values
          spc <- 20 * log10(spc)

        } else
        {
          # calculate MFCCs
          melfcc_output <-
            melfcc(
              clp,
              hoptime = (steps[2] - steps[1]) / clp@samp.rate,
              wintime =  wlg / clp@samp.rate,
              fbtype = fbt,
              spec_out = TRUE,
              frames_in_rows = FALSE,
              minfreq = bndpss[1] * 1000,
              maxfreq = bndpss[2] * 1000,
              ...
            )

          if (type == "mfcc")
            spc <- melfcc_output$cepstra

          if (type == "mel-auditory")
            spc <- melfcc_output$aspectrum

          # log amplitude values
          spc <- 20 * log10(spc + abs(min(spc)) + 10)
        }

        # replace inf by NA
        spc[is.infinite(spc)] <- NA

        return(spc)
      }

    # create function to calculate correlation between 2 spectrograms
    XC_FUN <- function(spc1, spc2, cm = cor.method) {
      # define short and long envelope for sliding one (short) over the other (long)
      if (ncol(spc1) > ncol(spc2)) {
        lg.spc <- spc1
        shrt.spc <- spc2
      } else {
        lg.spc <- spc2
        shrt.spc <- spc1
      }

      # get length of shortest minus 1 (1 if same length so it runs a single correlation)
      shrt.lgth <- ncol(shrt.spc) - 1

      # steps for sliding one signal over the other
      stps <- ncol(lg.spc) - ncol(shrt.spc)

      # set sequence of steps, if <= 1 then just 1 step
      if (stps <= 1)
        stps <- 1 else
        stps <- 1:stps

      # calculate correlations at each step
      cors <- sapply(stps, function(x, cor.method = cm) {
        warbleR::try_na(cor(
          c(lg.spc[, x:(x + shrt.lgth)]),
          c(shrt.spc),
          method = cm,
          use = 'pairwise.complete.obs'
        ))
      })

      # make negative values 0
      cors[cors < 0] <- 0

      return(cors)
    }

    # set cores cores
    if (Sys.info()[1] == "Windows" & cores > 1)
      cl <-
      parallel::makePSOCKcluster(getOption("cl.cores", cores)) else
      cl <- cores

    # get correlation
    corr_vector_list <-
      warbleR:::pblapply_wrblr_int(
        pbar = pb,
        X = 1:nrow(compare.matrix),
        cl = cl,
        FUN = function(e, cor.meth = cor.method) {
          # set bandpass to template frequency range
          bp <-
            c(templates$bottom.freq[templates$selection.id %in% compare.matrix$templates[e]], templates$top.freq[templates$selection.id %in% compare.matrix$templates[e]])

          spc_template <-
            spc_FUN(
              j = which(templates$selection.id == compare.matrix$templates[e]),
              pth = path,
              W = templates,
              hop = hop.size,
              wlg = wl,
              ovl = ovlp,
              w = wn,
              bndpss = bp,
              fbt = fbtype,
              ...
            )

          spc_file <-
            spc_FUN(
              j = compare.matrix$files[e],
              pth = path,
              hop = hop.size,
              wlg = wl,
              ovl = ovlp,
              w = wn,
              entire = TRUE,
              bndpss = bp,
              fbt = fbtype,
              ...
            )

          # get cross correlation
          corr_vector <-
            XC_FUN(spc1 = spc_template,
                   spc2 = spc_file,
                   cm = cor.meth)

          # get header to extract metadata
          file_header <-
            warbleR::read_sound_file(X = compare.matrix$files[e],
                                     path = path,
                                     header = TRUE)

          template_duration <-
            templates$end[templates$selection.id == compare.matrix$templates[e]] - templates$start[templates$selection.id == compare.matrix$templates[e]]


          output <- list(
            correlation.scores = corr_vector,
            template.duration = template_duration,
            file.duration = file_header$samples / file_header$sample.rate,
            sampling.freq = file_header$sample.rate
          )

          return(output)
        }
      )

    # add template and file names
    names(corr_vector_list) <-
      apply(compare.matrix, 1, paste, collapse = "/")

    # add call info
    corr_vector_list[[length(corr_vector_list) + 1]]  <- list(
      parameters = lapply(as.list(base::match.call())[-1], eval),
      call = base::match.call(),
      ohun.version =  packageVersion("ohun")
    )

    names(corr_vector_list)[length(corr_vector_list)] <- "call_info"

    # add class envelopes
    class(corr_vector_list) <- c("list", "template_correlations")

    return(corr_vector_list)

  }


##############################################################################################################

#' print method for class \code{template_correlations}
#'
#' @param x Object of class \code{template_correlations}, generated by \code{\link{template_correlator}}.
#' @param ...	 further arguments passed to or from other methods. Ignored when printing 'template_correlations' class objects.
#' @keywords internal
#'
#' @export
#' @returns Prints a summary of an object of class 'template_correlations'.


print.template_correlations <- function(x, ...) {
  cat(crayon::black(paste(
    "Object of class",
    crayon::bold("'template_correlations' \n")
  )))

  cat(crayon::silver(
    paste(
      "* The output of the following",
      crayon::italic("template_correlator()"),
      "call: \n"
    )
  ))

  cll <- paste0(deparse(x$call_info$call))
  cat(crayon::silver(crayon::italic(gsub("    ", "", cll), "\n")))

  # get file and template names
  files_templates <- sapply(names(x)[-length(x)], strsplit, "/")
  templates <- unique(sapply(files_templates, "[[", 1))
  files <- unique(sapply(files_templates, "[[", 2))

  cat(crayon::silver(
    paste("* Contains", length(x) - 1, "correlation score vector(s) from"),
    length(templates),
    "template(s):\n",
    paste(crayon::italic(utils::head(templates), collapse = " ")),
    if (length(templates) > 6)
      paste("... and", length(templates) - 6, "more") else
      ""
  ))

  cat(crayon::silver(
    paste("\n... and"),
    length(files),
    "sound files(s):\n",
    paste(crayon::italic(utils::head(files), collapse = " ")),
    if (length(files) > 6)
      paste("... and", length(files) - 6, "more") else
      ""
  ))

  # print ohun version
  cat(crayon::silver(
    paste0(
      "\n * Created by ",
      crayon::bold("ohun "),
      x$call_info$ohun.version
    )
  ))
}
