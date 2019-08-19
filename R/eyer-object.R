#' Returns empty eyer object
#'
#' @description
#' EyerObject is a list with predetermined fields for consistent functioning of other
#' functions within the Eyer package. Lots of functions in the eyer package can
#' work on passed "raw" data, such as fixation dataframes etc., but the benefit of
#' preprocessing and parsing data inside the eyer object is that the object
#' is inherently validated and processed and functions know what to expect. This
#' data science design has been implemented from similar approaches found in Matlab
#' and Python packages.
#'
#' EyerObject has the following fields
#'
#' - data
#'    - events
#'    - fixations
#'    - pupil
#'    - gaze
#' - start_time: start time of the first recording in datetime
#' - eyetracker: string with name of the recording device
#' - settings: list with eyetracker specific settings, suhc as frequency, recorded eye etc.
#'
#' Fixations have an obligatory columns: timestamps (s since start), position_X, position_y
#'
#' @return eyer object
#' @export
#'
#' @examples
EyerObject <- function(){
  obj <- list()
  obj$data <- list()
  obj$data$events <- data.frame()
  obj$data$fixations <- data.frame()
  obj$start_time <- numeric(0)
  obj$eyetracker <- character(0)
  obj$settings <- list()
  class(obj) <- append(class(obj), "eyer")
  return(obj)
}
