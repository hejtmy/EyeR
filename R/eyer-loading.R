#' Loads data from a folder. Loads preprocessed events, gaze and fixations files
#'
#' @description eyer allows saving of preprocessed data with the function `eyer_save()`
#' this function then allows to load directly those prepared files into its structure
#'
#' @param folder what directory should be searcher? Not recursive
#'
#' @return EyerObject with filled in data
#' @export
#'
#' @examples
load_eyer_data <- function(folder, silent = T, ...){
  obj <- EyerObject()
  #checks if there are already computed files
  ls_filepaths <- find_preprocessed_files(folder)
  #if (override) delete_preprocessed_files(ls_filepaths)
  obj <- load_preprocessed_files(obj, ls_filepaths, silent = silent)
  return(obj)
}

#' returns filepaths of preprocessed fixations and events
#'
#' @param folder what directory to search in? Expects one fixation and one event to be present
#'
#' @return list with "fixations" and "events" fileds containing filepaths
#' @examples
find_preprocessed_files <- function(folder){
  #actually search for it
  ls <- list()
  for(field in DATA_FIELDS){
    preprocessed_ptr <- paste0("_eyer_", field)
    ls[[field]] <- find_file_or_null(folder, preprocessed_ptr)
  }
  ls$info <- find_file_or_null(folder, "_eyer_info.json")
  return(ls)
}

#' @param folder where to look
#' @param ptr pattern to search for
#' @noRd
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

load_preprocessed_files <- function(obj, ls_filepaths, silent = F){
  for(field in c("fixations", "gaze", "diameter", "events")){
    if(!is.null(ls_filepaths[[field]])){
      if(!silent) cat("Loading preprocessed", field, "log at", ls_filepaths[[field]], "\n")
      obj$data[[field]] <- read.table(ls_filepaths[[field]], sep = ";", header = T)
    }
  }
  if(!is.null(ls_filepaths$info)){
    if(!silent) cat("Loading preprocessed info log", ls_filepaths$gaze)
    obj$info <- jsonlite::read_json(ls_filepaths$info, simplifyVector = T)
  }
  return(obj)
}

delete_preprocessed_files <- function(ls_filepaths){
  if(file.exists(ls_filepaths$fixations)) {
    cat("Removing preprocessed fixations log", ls_filepaths$fixations)
    file.remove(ls_filepaths$fixations)
  }
  if(file.exists(ls_filepaths$events)) {
    cat("Removing preprocessed events log", ls_filepaths$events)
    file.remove(ls_filepaths$events)
  }
}
