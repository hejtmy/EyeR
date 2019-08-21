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
#'    - diameter
#'    - gaze
#' - info: list with eyetracker specific settings, suhc as recording frequency, recorded eye etc.
#'    - start_time: start time of the first recording in datetime
#'    - resolution: monitor resolution as a list with width, height fields
#'    - eyetracker: string with name of the recording device
#'
#' Fixations have an obligatory columns: time (s since start), x, y, duration
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
  obj$data$gaze <- data.frame()
  obj$info <- list(resolution = list(width=NA, height=NA),
                   eyetracker = character(0),
                   start_time = numeric(0))
  class(obj) <- append(class(obj), "eyer")
  return(obj)
}

DATA_FIELDS <- c("gaze", "fixations", "events", "diameter")

#' Returns if passed object is valid eyer object
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
is_valid_eyer <- function(obj){
  if(!("eyer" %in% attributes(obj)$class)){
    warning("object doesn't have eyer class")
    return(FALSE)
  }
  if(length(obj$info$start_time) != 1){
    warning("object doesn't have valid start time")
    return(FALSE)
  }
  ## FIXATIONS
  # Chcks validity of fixations in case there is one
  if(nrow(obj$data$fixations) > 0){
    required_fixation_columns <- c("x", "y", "time", "duration")
    if(!all(required_fixation_columns %in% names(obj$data$fixations))){
      warning("fixations don't have required", required_fixation_columns, " columns")
      return(FALSE)
    }
  }
  ## GAZE
  if(nrow(obj$data$gaze) > 0){
    required_gaze_columns <- c("x", "y", "time")
    if(!all(required_gaze_columns %in% names(obj$data$gaze))){
      warning("gaze positions don't have required", required_gaze_columns, " columns")
      return(FALSE)
    }
  }
  return(TRUE)
}

