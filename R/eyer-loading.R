#' Title
#'
#' @param dir
#' @param override
#' @param eyetracker
#'
#' @return
#' @export
#'
#' @examples
load_eyetracker_data <- function(dir, override, eyetracker="SR 1000 ASC"){
  #checks if there are already computed files
  ls_filepaths <- find_preprocessed_files(dir)
  if (override) delete_preprocessed_files(ls_filepaths)
  obj <- EyerObject()
  obj$data <- load_preprocessed_files(ls_filepaths)
  #if we are still missing some data we recompute it

  file <- find_unprocessed_file(dir)
  if (!is_loaded(obj)){
    ls <- load_unprocessed_file(file)
  }
  obj$resolution <- read_eye_resolution(ls$file)
  return(obj)
}

#' Title
#'
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
find_preprocessed_files <- function(dir){
  #actually search for it
  ptr <- paste(dir, file, "_fixations.txt", sep = "")
  fixations_filepath <- list.files(dir, pattern = ptr, full.names = T)
  ptr <- paste(dir, file, "_events.txt", sep = "")
  events_filepath <- list.files(dir, pattern = ptr, full.names = T)
  return(list(fixations=fixations_filepath, events=events_filepath))
}

#' Finds and loads data from a directory
#'
#' @param dir
#' @param type
#'
#' @return
#' @export
#'
#' @examples
open_unprocessed_file <- function(dir, eyetracker="SR 1000 ASC"){
  file <- find_unprocessed_file(dir, eyetracker)
  data <- load_unprocessed_file(file, eyetracker)
  return(list(file=file, data=data))
}

#' Returns list of eyetracker files of given eyetracker
#'
#' @param dir where to look for
#' @param eyetracker what eyetracker file are we dealing with
#'
#' @return
#' @export
#'
#' @examples
find_unprocessed_file <- function(dir, eyetracker="SR 1000 ASC"){
  ptr <- paste(dir, ".asc", sep = "")
  filepath <- list.files(dir, pattern = ptr, full.names = T)
  if(!file.exists(filepath)){
    warning("There is no log in destination")
    return(NULL)
  }
  return(filepath)
}

#' Title
#'
#' @param filepath
#' @param type
#'
#' @return
#' @export
#'
#' @examples
load_unprocessed_file <- function(filepath, eyetracker="SR 1000"){
  #check for log existance
  events <- read_eye_events(filepath, eyetracker)
  fixations <- read_eye_fixations(filepath, eyetracker)
  return(ls(events=events, fixations=fixations))
}


load_preprocessed_files <- function(ls_filepaths){
  ls <- list(fixations=NULL, events=NULL)
  if(file.exists(ls_filepaths$fixations)) {
    SmartPrint(c("Loading preprocessed fixations log", ls_filepaths$fixation))
    fixations <- fread(ls_filepaths$fixation, sep=";", header = T)
  }
  if(file.exists(ls_filepaths$fixations)){
    SmartPrint(c("Loading preprocessed events log", ls_filepaths$events))
    events <- fread(ls_filepaths$events, sep=";", header = T)
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
