#' Returns empty eyelinkr object
#'
#' @return Eyelinkr object
#' @export
#'
#' @examples
EyelinkrObject <- function(){
  obj <- list()
  obj$data <- list()
  obj$data$events <- NULL
  obj$data$fixations <- NULL
  class(obj) <- append(class(obj), "eyelinkr")
  return(obj)
}
