#' Plots monitor boundaries
#'
#' @details wrapper around `geom_area_boundary` with some default values
#'
#' @param width monitor width as a numeric value
#' @param height monitor height as a numeric value
#' @param alpha rectangle alpha, **default** is 0
#' @param size rectangle line size, **default** is 1.5
#' @param color rectangle color, **default** is red
#' @param ... optional `geom_rect` parameters
#'
#' @return ggplot geom
#' @export
#'
#' @examples
geom_monitor_boundaries <- function(width, height, alpha = 0, size = 1.5, color = 'red', ...){
  return(list(geom_rect(aes(xmin=0, xmax=width, ymin=0, ymax=height),
                        inherit.aes = F, alpha = alpha,
                        size = size, color = color, ...)))
}

#' Plots area boundaries
#'
#' @details Wrapper around `geom_rect` to be used with *area* functions
#'
#' @param ls list with xmin, xmax, ymin, ymax fields
#' @param alpha rectangle alpha, **default** is 0.2
#' @param size rectangle line size, **default** is 1
#' @param color rectangle color, **default** is 'black'
#' @param fill rectangle fill, **default** is 'grey20'
#' @param ... optional `geom_rect` parameters
#'
#' @return ggplot geom
#' @export
#'
#' @examples
geom_area_boundaries <- function(ls, alpha = 0.2, size = 1,
                               color = "black", fill = "grey20", ...){
  return(list(geom_rect(aes(xmin=ls$xmin, xmax=ls$xmax, ymin=ls$ymin, ymax=ls$ymax),
                        inherit.aes = F, alpha = alpha,
                        size = size, color = color, fill = fill,
                        ...)))
}
