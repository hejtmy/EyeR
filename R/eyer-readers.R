#' Reads events from given file
#'
#' @param filepath path to the raw file
#' @param eyetracker which eyetrakcer was used? See readme for instructions
#' @export
read_eye_events <- function(filepath, eyetracker){
  text <- readLines(filepath)
  if(eyetracker == "SR 1000") events <- SR1000.read_eye_events(text)
  return(events)
}
#' Reads fixations from given file
#'
#' @param filepath path to the raw file
#' @param eyetracker which eyetrakcer was used? See readme for instructions
#' @export
read_eye_fixations <- function(filepath, eyetracker){
  text <- readLines(filepath)
  if(eyetracker == "SR 1000") fixations <- SR1000.read_eye_fixations(text)
  return(fixations)
}

#' Reads movements from given file
#'
#' @param filepath path to the raw file
#' @param eyetracker which eyetrakcer was used? See readme for instructions
#'
#' @export
read_eye_movements <- function(filepath, eyetracker){
  text <- readLines(filepath)
  if(eyetracker == "SR 1000") return(SR1000.read_eye_movements(text))
  warning('Provided eyetracker type is not supported')
}

#' Reads eyetracker resolution from given file
#'
#' @param filepath path to the raw file
#' @param eyetracker which eyetrakcer was used? See readme for instructions
#'
#' @return
#' @export
#'
#' @examples
read_resolution <- function(filepath, eyetracker){
  if(eyetracker == "SR 1000") resolution <- SR1000.read_resolution(filepath)
  return(resolution)
}

#' Reads eyetracker calibration infromation from given file
#'
#' @param filepath path to the raw file
#' @param eyetracker which eyetrakcer was used? See readme for instructions
#'
#' @return
#' @export
#'
#' @examples
read_calibrations <- function(filepath, eyetracker){
  text <- readLines(filepath)
  if(eyetracker == "SR 1000") resolution <- SR1000.read_calibrations(text, 100)
  return(resolution)
}
