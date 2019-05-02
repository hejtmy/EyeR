#' Returns if the data is loaded
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
is_data_loaded <- function(obj){
  UseMethod("is_data_loaded")
}

#' Returns if the eylinker object has both evens and fixations loaded
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
is_data_loaded.eyer <- function(obj){
  return(!((is.null(obj$events)) | (is.null(obj$fixations))))
}
