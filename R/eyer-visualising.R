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


#' PLot fixations data
#'
#' @param obj
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_fixations <- function(obj, ...){
  UseMethod("plot_fixations")
}

plot_fixations.eyer <- function(obj, duration = T, ...){
  df <- obj$data$fixations
  plt <- ggplot(df, aes(x, y)) + theme_minimal()
  if(duration) plt <- plt + geom_point(aes(color=duration, size=duration), ...) + scale_size(guide=F)
  if(!duration) plt <- plt + geom_point(...)
  return(plt)
}
