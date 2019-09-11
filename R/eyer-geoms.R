#' Plots monitor boundaries
#'
#' @details wrapper around `geom_area_boundary` with some default values
#'
#' @param width monitor width as a numeric value
#' @param height monitor height as a numeric value
#' @param alpha rectangle alpha
#' @param size rectangle line size
#' @param color rectangle color
#' @param ... optional `geom_rect` parameters
#'
#' @return ggplot geom
#' @export
#'
#' @examples
geom_monitor_boundaries <- function(width, height, alpha = 0, size = 1.5, color = 'red', ...){
  geoms <- list(geom_rect(aes(xmin=0, xmax=width, ymin=0, ymax=height),
                          inherit.aes = F, alpha = alpha,
                          size = size, color = color, ...))
  return(geoms)
}

#' Plots area boundaries
#'
#' @details Wrapper around `geom_rect` to be used with *area* functions
#'
#' @param area \code\link{AreaObject}}
#' @param alpha rectangle alpha
#' @param size rectangle line size
#' @param color rectangle color
#' @param fill rectangle fill
#' @param ... optional `geom_rect` parameters
#'
#' @return ggplot geom
#' @export
#'
#' @examples
geom_area_boundaries <- function(area, alpha = 0.01, size = 1, color = "black", fill = "grey20", ...){
  points <- area$points
  if(area$type == "square") geoms <- geom_area_boundaries_square(points, alpha, size, color, fill, ...)
  return(geoms)
}

geom_area_boundaries_square <- function(points, alpha, size, color, fill, ...){
  geoms <- list(geom_rect(aes(xmin=points$xmin, xmax=points$xmax, ymin=points$ymin, ymax=points$ymax),
                          inherit.aes = FALSE, alpha = alpha,
                          size = size, color = color, fill = fill,
                          ...))
  return(geoms)
}
