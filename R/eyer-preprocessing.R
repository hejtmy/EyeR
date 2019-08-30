#' Approximates x, y resolution from given coordinates to designated
#'
#' @param obj Object to do transformations on
#' @param original defined as a list with width and height in pixesls.
#' Ex: list(width=1920, height=1080)
#' @param target defined as a list with width and height in pixesls.
#' Ex: list(width=1920, height=1080)
#'
#' @return data frame with fixations with changed resolution. Can still yield out of bounds
#' @export
#'
#' @examples
change_resolution <- function(obj, original, target, ...){
  UseMethod("change_resolution")
}

#' @export
change_resolution.eyer <- function(obj, original, target){
  if(nrow(obj$data$fixations) > 0) obj$data$fixations <- change_resolution.data.frame(obj$data$fixations, original, target)
  if(nrow(obj$data$gaze) > 0) obj$data$gaze <- change_resolution.data.frame(obj$data$gaze, original, target)
  return(obj)
}

#' @export
change_resolution.data.frame <- function(df, original, target){
  df$x <- round(df$x/original$width * target$width)
  df$y <- round(df$y/original$height * target$height)
  return(df)
}
#' Removes fixations out of disp_resolution boundary
#'
#' @param df_fixations fixation table
#' @param disp_resolution defined as a list with width and height in pixesls.
#' Ex: list(width=1920, height=1080)
#'
#' @return
#' @export
#'
#' @examples
remove_out_of_bounds_fixations <- function(df_fixations, disp_resolution = list(width = 1920, height = 1080)){
  df_fixations <- df_fixations[(df_fixations$x < disp_resolution$width) &
                                 (df_fixations$y < disp_resolution$height), ]
  df_fixations <- df_fixations[df_fixations$x > 0 & df_fixations$y > 0, ]
  return(df_fixations)
}

#' Adds new column to the fixations data frame with information about a fixation
#' being within area bounds
#'
#' @param df_fixations fixations
#' @param areas list of area lists. Each area list is a list of X and y vectors
#' of length 2. Ex: list(x=c(0,10), y=c(0,10))
#'
#' @return
#' @export
#'
#' @examples
add_screen_area_fixations <- function(df_fixations, areas){
  #TODO - this is not working
  df_fixations$area <- ""
  for (area in areas){
    df_fixations[is_between(df_fixations$x, area$x[1], ar$x[2]) &
                   is_between(df_fixations$y, area$y[1], area$y[2]), "area"] <- area$name
  }
  return(df_fixations)
}

