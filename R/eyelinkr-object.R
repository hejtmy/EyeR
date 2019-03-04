#' Returns empty eyer object
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
