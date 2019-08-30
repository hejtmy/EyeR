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
  df <- df[seq(1, nrow(df), frequency), ]
  plt <- ggplot(df, aes(x, y)) + geom_point(...)
  return(plt)
}
