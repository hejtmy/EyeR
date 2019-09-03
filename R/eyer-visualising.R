#' Plots gaze data
#'
#' @param obj object to plot the data with
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_gaze <- function(obj, ...){
  UseMethod("plot_gaze")
}

#' Plots gaze data
#'
#' @param obj eyer object
#' @param downsample picks only every nth element due to gaze data being huge
#' @param ...
#'
#' @return ggplot2 plot
#' @export
#'
#' @examples
plot_gaze.eyer <- function(obj, downsample = 10, ...){
  df <- obj$data$gaze
  if(downsample > 1) df <- downsample(df, downsample)
  plt <- ggplot(df, aes(x, y)) + geom_point(...) + theme_minimal()
  return(plt)
}

#' Plot fixations data
#'
#' @param obj object with the fixations data
#' @param ...
#'
#' @return ggplot2 plot
#' @export
#'
#' @examples
plot_fixations <- function(obj, ...){
  UseMethod("plot_fixations")
}

#' Plot fixations and their durations
#'
#' @param obj Eyer object with fixations loaded
#' @param duration if the duration should be plotted. Visualised by color and size. **default** is TRUE
#' @param ...
#'
#' @return ggplot2 plot
#' @export
#'
#' @examples
plot_fixations.eyer <- function(obj, duration = T, ...){
  df <- obj$data$fixations
  plt <- ggplot(df, aes(x, y)) + theme_minimal()
  if(duration) plt <- plt + geom_point(aes(color=duration, size=duration), ...) + scale_size(guide=F)
  if(!duration) plt <- plt + geom_point(...)
  return(plt)
}

#' Plot heatmap of gaze information
#' @description Wrapper around \code{\link{plot_eye_heatmap}} to plot gaze information
#' @param obj object with gaze data
#' @param ... optional ggplot arguments
#'
#' @return ggplot plot
#' @export
#'
#' @examples
plot_gaze_heatmap <- function(obj, ...){
  UseMethod("plot_gaze_heatmap")
}

#' Plot heatmap of gaze information
#'
#' @param obj EyerObject with validgaze data
#' @param ... additional ggplot arguments
#'
#' @export
plot_gaze_heatmap.eyer <- function(obj, ...){
  df <- obj$data$gaze
  return(plot_eye_heatmap(df$x, df$y, weights = NULL, ...))
}

#' Generic function to plot eye heatmap
#'
#' @description Plots heatmap using ggplot stat_density2d function and using x, y and weights coordinates
#'
#' @param x x coordinates
#' @param y y coordinates
#' @param weights optional parameter signifiing durations of x, y coordinates in case they are not of the same importance/ value
#' @param ... optional ggplot parameters
#'
#' @return ggplot of stat density
#' @export
#'
#' @examples
plot_eye_heatmap <- function(x, y, weights = NULL, ...){
  #validate - x and y and weights same length
  df <- data.frame(x, y)
  plt <- ggplot(df, aes(x, y)) +
    stat_density2d(aes(fill = ..level..), geom = "polygon", ...) +
    scale_fill_viridis_c() + theme_minimal()
  return(plt)
}


#' Plots monitor boundaries around data
#'
#' @details creates ggplot geom which plots a geom_rect given specific resolution infromation in the info field
#'
#' @description default geom properties are size = 1, alpha = 0 and color = 'red'. You can overwrite these in teh ...
#' argument.
#'
#' @param obj EyerObject with `obj$info$resolution` field
#' @param ... ggplot options for geom_rect
#'
#' @return ggplot geom
#' @export
#'
#' @examples
geom_eyer_monitor <- function(obj, ...){
  res <- obj$info$resolution
  if(is.null(res)){
    warning("passed EyerObject doens't have obj$info$resolution field")
    return(list())
  }
  width <- res$width
  height <- res$height
  if(any(is.null(width), is.null(height), is.na(width), is.na(height))){
    warning("passed obj$info$resolution doesn't have valid width, height fields")
    return(list())
  }
  return(geom_monitor_boundaries(width, height, ...))
}
