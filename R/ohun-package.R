#' ohun: Optimizing sound event detection
#'
#' ohun is intended to facilitate the automated detection of sound events, providing functions to diagnose and optimize detection routines. Detections from other software can also be explored and optimized.
#'
#' The main features of the package are:
#'   \itemize{
#'   \item The use of reference annotations for detection optimization and diagnostic
#'   \item The use of signal detection theory diagnostic parameters to evaluate detection performance
#'   \item The batch processing of sound files for improve computational performance
#'   }
#'
#' The package offers functions for:
#'   \itemize{
#'   \item Energy-based detection
#'   \item Template-based detection
#'   \item Diagnose detection precision
#'   \item Optimize detection routines based on reference annotations
#'   }
#'
#' All functions allow the parallelization of tasks, which distributes the tasks among several processors to improve computational efficiency. The package works on sound files in '.wav', '.mp3', '.flac' and '.wac' format.
#'
#' @import warbleR
#' @import rjson
#' @import RCurl
#' @import pbapply
#' @import tuneR
#' @import seewave
#' @import fftw
#' @import graphics
#' @import grDevices
#' @import parallel
#' @import utils
#' @import methods
#' @import stats
#' @import Sim.DiffProc
#' @importFrom rlang call_args
#' @importFrom viridis viridis inferno
#' @importFrom knitr kable
#' @importFrom crayon silver bold italic
#' @importFrom igraph max_bipartite_match E as_data_frame graph_from_incidence_matrix
#' @importFrom methods is
#' @importFrom tuneR writeWave
#' @importFrom stats cor weighted.mean
#' @importClassesFrom tuneR Wave
#' @author Marcelo Araya-Salas
#'
#'   Maintainer: Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'
#' @docType package
#' @name ohun
#' @details License: GPL (>= 2)
NULL
#> NULL
#'
