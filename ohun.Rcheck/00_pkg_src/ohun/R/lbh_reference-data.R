#' Example data frame of a selection table including all sound events of interests.
#'
#' A data frame containing the start, end, low and high frequency of
#' \emph{Phaethornis longirostris} (Long-billed Hermit) songs from the 2
#' example sound files included in this package ('lbh_1' and 'lbh_2'). These two files are clips extracted from the xeno-canto's '154138' and '154129' recordings respectively.
#'
#' @format A data frame with 19 rows and 6 variables: \describe{
#'  \item{sound.files}{recording names}
#'  \item{selec}{selection numbers within recording}
#'  \item{start}{start times of selected sound event}
#'  \item{end}{end times of selected sound event}
#'  \item{bottom.freq}{lower limit of frequency range}
#'  \item{top.freq}{upper limit of frequency range}
#' }
#'
#' @usage data(lbh_reference)
#'
#' @source Marcelo Araya-Salas, ohun
#'
#' @description \code{lbh_reference} is a data frame containing the start, end, bottom and top frequency of all songs in 'lbh_1.wav' and 'lbh_2.wav' recordings. #'
"lbh_reference"
