#' Loads data from a folder. Loads preprocessed events and fixations files,
#' or raw files if override is set to TRUE or if there are no preprocessed files
#'
#' @param folder what directory should be searcher? Not recursive
#' @param override should we delete and override existing preprocessed files?
#' @param eyetracker what type of eyetrakcer was used? See readme for instructions
#'
#' @return EyerObject with filled in data
#' @export
#'
#' @examples
load_eyetracker_data <- function(folder, override, eyetracker="SR 1000", ...){
  #checks if there are already computed files
  ls_filepaths <- find_preprocessed_files(folder)
  if (override) delete_preprocessed_files(ls_filepaths)
  obj <- EyerObject()
  obj$data <- load_preprocessed_files(ls_filepaths)

  #if we are still missing some data we recompute it
  file <- find_unprocessed_file(folder)
  if (!is_loaded(obj)) obj$data <- load_unprocessed_file(file)
  obj$resolution <- read_resolution(file, eyetracker)
  return(obj)
}

#' Finds and loads data from a directory
#'
#' @param folder what directory to search in? Expects one file to be present
#' @param eyetracker what type of eyetrakcer was used? See readme for instructions
#'
#' @return returns loaded data from load_unprocessed_file function
#' @export
#'
#' @examples
open_unprocessed_file <- function(folder, eyetracker="SR 1000"){
  filepath <- find_unprocessed_file(folder, eyetracker)
  data <- load_unprocessed_file(filepath, eyetracker)
  return(data)
}

#' Loads raw file and returns list with evens and fixations
#'
#' @param filepath path to the file
#' @param eyetracker what type of eyetrakcer was used? See readme for instructions
#'
#' @return
#' @export
#'
#' @examples
load_unprocessed_file <- function(filepath, eyetracker="SR 1000"){
  #check for log existance
  events <- read_eye_events(filepath, eyetracker)
  fixations <- read_eye_fixations(filepath, eyetracker)
  return(list(events = events, fixations = fixations))
}

#' returns filepaths of preprocessed fixations and events
#'
#' @param folder what directory to search in? Expects one fixation and one event to be present
#'
#' @return list with "fixations" and "events" fileds containing filepaths
#' @examples
find_preprocessed_files <- function(folder){
  #actually search for it
  fixations_filepath <- find_file_or_null(folder, "_eyer_fixations.txt")
  events_filepath <- find_file_or_null(folder, "_eyer_events.txt")
  return(list(fixations = fixations_filepath, events = events_filepath))
}

#' Returns list of eyetracker files of given eyetracker
#'
#' @param folder where to look for
#' @param eyetracker what eyetracker file are we dealing with
#'
#' @return
#' @examples
find_unprocessed_file <- function(folder, eyetracker="SR 1000"){
  filepath <- find_file_or_null(folder, ".asc")
  if(is.null(filepath) || !file.exists(filepath)){
    warning("There is no log in destination ", folder)
    return(NULL)
  }
  return(filepath)
}

#' @param folder where to look
#' @param ptr pattern to search for
find_file_or_null <- function(folder, ptr, allow_multiples = F){
  ptr <- paste("*", ptr, sep = "")
  filepath <- list.files(folder, pattern = ptr, full.names = T)
  if(length(filepath) == 0) return(NULL)
  if(length(filepath) == 1) return(filepath)
  if(allow_multiples){
    return(filepath)
  } else {
    return(NULL)
  }
}

load_preprocessed_files <- function(ls_filepaths){
  ls <- list(fixations = data.frame(), events = data.frame())
  if(!is.null(ls_filepaths$fixations) && file.exists(ls_filepaths$fixations)) {
    SmartPrint(c("Loading preprocessed fixations log", ls_filepaths$fixation))
    fixations <- read.table(ls_filepaths$fixation, sep = ";", header = T)
  }
  if(!is.null(ls_filepaths$events) && file.exists(ls_filepaths$events)){
    SmartPrint(c("Loading preprocessed events log", ls_filepaths$events))
    events <- read.table(ls_filepaths$events, sep = ";", header = T)
  }
  return(ls)
}

delete_preprocessed_files <- function(ls_filepaths){
  if(file.exists(ls_filepaths$fixations)) {
    SmartPrint(c("Removing preprocessed fixations log", ls_filepaths$fixations))
    file.remove(ls_filepaths$fixations)
  }
  if(file.exists(ls_filepaths$events)) {
    SmartPrint(c("Removing preprocessed events log", ls_filepaths$events))
    file.remove(ls_filepaths$events)
  }
}

is_loaded <- function(obj){
  return(all(nrow(obj$data$fixations) > 0,
             nrow(obj$data$events) > 0
  ))
}
