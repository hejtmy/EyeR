#' Reads fixations from given file
#'
#' @param filepath
#' @param eyetracker eyetracker type
#' @export
read_eye_events <- function(filepath, eyetracker){
  text <- readLines(filepath)
  if(eyetracker == "SR 1000") events <- SR1000.read_eye_events(text)
  return(events)
}
#' Reads fixations from given file
#'
#' @param filepath
#' @param eyetracker eyetracker type
#' @export
read_eye_fixations <- function(filepath, eyetracker){
  text <- readLines(filepath)
  if(eyetracker == "SR 1000") fixations <- SR1000.read_eye_fixations(text)
  return(fixations)
}

#' Reads movements from given file
#'
#' @param filepath
#' @param eyetracker eyetracker type
#' @export
read_eye_movements <- function(filepath, eyetracker){
  text <- readLines(filepath)
  if(eyetracker == "SR 1000") return(SR1000.read_eye_movements(text))
  warning('Provided eyetracker type is not supported')
}

