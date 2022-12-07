#' Summarize detection diagnostics
#'
#' \code{summarize_diagnostic} summarizes detection diagnostics
#' @usage summarize_diagnostic(diagnostic, time.diagnostics = FALSE)
#' @param diagnostic A data frame with the reference selections (start and end of the sound events) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end".
#' @return A data frame, typically the output of a detection optimization function (\code{\link{diagnose_detection}}, \code{\link{optimize_energy_detector}}, \code{\link{optimize_template_detector}}) including the following detection performance diagnostics:
#' \itemize{
#'  \item \code{total.detections}: total number of detections
#'  \item \code{true.positives}: number of sound events in 'reference' that correspond to any detection. Matching is defined as some degree of overlap in time. In a perfect detection routine it should be equal to the number of rows in 'reference'.
#'  \item \code{false.positives}: number of detections that don't match (i.e. don't overlap with) any of the sound events in 'reference'. In a perfect detection routine it should be 0.
#'  \item \code{false.negatives}: number of sound events in 'reference' that were not detected (not found in 'detection'. In a perfect detection routine it should be 0.
#'  \item \code{split.positives}: number of sound events in 'reference' that were overlapped by more than 1 detection (i.e. detections that were split). In a perfect detection routine it should be 0.
#'  \item \code{merged.positives}: number of sound events in 'reference' that were overlapped by a detection that also overlaps with other sound events in 'reference' (i.e. sound events that were merged into a single detection). In a perfect detection routine it should be 0.
#'  \item \code{mean.duration.true.positives}: mean duration of true positives (in s). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{mean.duration.false.positives}: mean duration of false positives (in ms). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{mean.duration.false.negatives}: mean duration of false negatives (in ms). Only included when \code{time.diagnostics = TRUE}.
#'  \item \code{overlap.to.true.positives}: ratio of the time overlap of true positives in 'detection' with its corresponding reference sound event to the duration of the reference sound event.
#'  \item \code{proportional.duration.true.positives}: ratio of duration of true positives to the duration of sound events in 'reference'. In a perfect detection routine it should be 1. Based only on true positives that were not split or merged.
#'  \item \code{duty.cycle}: proportion of a sound file in which sounds were detected. Only included when \code{time.diagnostics = TRUE} and \code{path} is supplied. Useful when conducting energy-based detection as a perfect detection can be obtained with a very low amplitude threshold, which will detect everything, but will produce a duty cycle close to 1.
#'  \item \code{recall}: Proportion of sound events in 'reference' that were detected. In a perfect detection routine it should be 1.
#'  \item \code{precision}: Proportion of detections that correspond to sound events in 'reference'. In a perfect detection routine it should be 1.
#'  \item \code{f1.score}: Combines recall and precision as the harmonic mean of these two. Provides a single value for evaluating performance. In a perfect detection routine it should be 1.
#'  }
#' @param time.diagnostics Logical argument to control if diagnostics related to the duration of the sound events ("mean.duration.true.positives", "mean.duration.false.positives", "mean.duration.false.negatives" and "proportional.duration.true.positives") are returned (if \code{TRUE}). Default is \code{FALSE}.
#' @export
#' @name summarize_diagnostic
#' @details The function summarizes a detection diagnostic data frame in which diagnostic parameters are shown split by (typically) a categorical column, usually sound files. This function is used internally by \code{\link{diagnose_detection}}.
#' @examples
#' {
#' # load example selection tables
#'
#' data("lbh_reference")
#'
#' # run diagnose_detection() by sound file
#' diag <- diagnose_detection(reference = lbh_reference,
#' detection = lbh_reference[-1, ], by.sound.file = TRUE)
#'
#' # summarize
#' summarize_diagnostic(diagnostic = diag)
#'
#' # should be the same as this:
#' diagnose_detection(reference = lbh_reference,
#' detection = lbh_reference[-1, ], by.sound.file = FALSE)
#' }
#' @seealso \code{\link{diagnose_detection}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references {
#' Araya-Salas, M. (2021), ohun: diagnosing and optimizing automated sound event detection. R package version 0.1.0.
#' }
# last modification on aug-19-2021 (MAS)
summarize_diagnostic <-
  function(diagnostic, time.diagnostics = FALSE) {
    # basic columns required in 'diagnostic'
    basic_colms <-
      c(
        "total.detections",
        "true.positives",
        "false.positives",
        "false.negatives",
        "split.positives",
        "merged.positives",
        "overlap.to.true.positives",
        "recall",
        "precision",
        "f1.score"
      )

    #check diagnostic
    if (any(!(basic_colms %in% colnames(diagnostic))))
      stop2(paste(
        paste(basic_colms[!(basic_colms %in% colnames(diagnostic))], collapse =
                ", "),
        "column(s) not found in 'diagnostics'"
      ))

    # get extra column names (ideally should include tuning parameters)
    extra_colms <-
      setdiff(colnames(diagnostic), c(
        basic_colms,
        c(
          "sound.files",
          "mean.duration.true.positives",
          "mean.duration.false.positives",
          "mean.duration.false.negatives",
          "proportional.duration.true.positives",
          "duty.cycle"
        )
      ))

    # create column combining all extra columns
    diagnostic$..combined.extra.colms <- if (length(extra_colms) > 0)
      apply(diagnostic[, extra_colms, drop = FALSE], 1, paste, collapse = "~>~") else
      "1"

    # get which extra columns were numeric
    if (length(extra_colms) > 0)
      numeric_colms <-
      sapply(diagnostic[, extra_colms, drop = FALSE], is.numeric)

    # switch to FALSE if no time columns
    if (is.null(diagnostic$mean.duration.true.positives))
      time.diagnostics <- FALSE

    summ_diagnostic_l <-
      lapply(unique(diagnostic$..combined.extra.colms), function(x) {
        # subset for each combination
        Y <- diagnostic[diagnostic$..combined.extra.colms == x,]

        # summarize across sound files
        summ_diagnostic <- data.frame(
          total.detections = sum(Y$true.positives, na.rm = TRUE),
          true.positives = sum(Y$true.positives, na.rm = TRUE),
          false.positives = sum(Y$false.positives, na.rm = TRUE),
          false.negatives = sum(Y$false.negatives, na.rm = TRUE),
          split.positives = sum(Y$split.positives, na.rm = TRUE),
          merged.positives = sum(Y$merged.positives, na.rm = TRUE),
          overlap.to.true.positives = if (any(!is.na(Y$overlap.to.true.positives)))
            stats::weighted.mean(
              x = Y$overlap.to.true.positives,
              w = Y$true.positives,
              na.rm = TRUE
            ) else
            NA,
          ..combined.extra.colms = x,
          stringsAsFactors = FALSE
        )

        # add time diagnostics
        if (time.diagnostics) {
          summ_diagnostic$mean.duration.true.positives <-
            if (any(!is.na(Y$mean.duration.true.positives)))
              round(
                stats::weighted.mean(
                  x = Y$mean.duration.true.positives,
                  w = Y$true.positives,
                  na.rm = TRUE
                ),
                0
              ) else
            NA
          summ_diagnostic$mean.duration.false.positives <-
            if (any(!is.na(Y$mean.duration.false.positives)))
              round(
                stats::weighted.mean(
                  x = Y$mean.duration.false.positives,
                  w = Y$true.positives,
                  na.rm = TRUE
                ),
                0
              ) else
            NA
          summ_diagnostic$mean.duration.false.negatives <-
            if (any(!is.na(Y$mean.duration.false.negatives)))
              round(
                stats::weighted.mean(
                  x = Y$mean.duration.false.negatives,
                  w = Y$true.positives,
                  na.rm = TRUE
                ),
                0
              ) else
            NA
          summ_diagnostic$proportional.duration.true.positives <-
            if (any(!is.na(Y$proportional.duration.true.positives)))
              stats::weighted.mean(
                x = Y$proportional.duration.true.positives,
                w = Y$true.positives,
                na.rm = TRUE
              ) else
            NA

          if (any(names(diagnostic) == "duty.cycle"))
            summ_diagnostic$duty.cycle <-
            mean(Y$duty.cycle, na.rm = TRUE)
        }

        # add recall precision and f1.score at the end
        summ_diagnostic$recall <-
          sum(Y$true.positives, na.rm = TRUE) / (sum(Y$true.positives, na.rm = TRUE) + sum(Y$false.negatives, na.rm = TRUE))
        summ_diagnostic$precision <-
          if (any(Y$precision != 0))
            (sum(Y$true.positives, na.rm = TRUE) / (sum(Y$total.detections, na.rm = TRUE))) else
          0
        summ_diagnostic$f1.score <-
          2 * ((summ_diagnostic$precision * summ_diagnostic$recall) / (summ_diagnostic$precision + summ_diagnostic$recall)
          )

        # replace NaNs with NA
        for (i in 1:ncol(summ_diagnostic))
          if (is.nan(summ_diagnostic[, i]))
            summ_diagnostic[, i] <- NA

        return(summ_diagnostic)
      })

    # put all in a single data frame
    summ_diagnostics_df <- do.call(rbind, summ_diagnostic_l)

    # add extra columns data
    if (length(unique(diagnostic$..combined.extra.colms)) > 1) {
      # extract extra columns as single columns
      extra_colms_df <-
        do.call(rbind,
                strsplit(summ_diagnostics_df$..combined.extra.colms, "~>~"))

      # add column names
      colnames(extra_colms_df) <- extra_colms

      # convert numeric columns
      if (any(numeric_colms)) {
        extra_num_colms_df <-
          as.data.frame(apply(extra_colms_df[, numeric_colms, drop = FALSE], 2, as.numeric))

        # add non-numeric columns
        if (any(!numeric_colms)) {
          non_num_colms_df <- extra_colms_df[,!numeric_colms, drop = FALSE]
          colnames(non_num_colms_df) <-
            names(numeric_colms)[!numeric_colms]
          extra_colms_df <-
            cbind(non_num_colms_df, extra_num_colms_df)

        } else
          extra_colms_df <- extra_num_colms_df
      }

      # put all together
      summ_diagnostics_df <-
        cbind(extra_colms_df, summ_diagnostics_df)
    }

    # remove column with all extra columns info
    summ_diagnostics_df$..combined.extra.colms <- NULL


    return(summ_diagnostics_df)
  }
