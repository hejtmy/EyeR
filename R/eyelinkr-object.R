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
#'    - gaze
#'
#' @return EyeR object
#' @export
#'
#' @examples
EyerObject <- function(){
  obj <- list()
  obj$data <- list()
  obj$data$events <- NULL
  obj$data$fixations <- NULL
  class(obj) <- append(class(obj), "eyer")
  return(obj)
}
