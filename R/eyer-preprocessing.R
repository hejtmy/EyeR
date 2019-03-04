#' Title
#'
#' @param df_events
#' @param eyetracker
#'
#' @return
#' @export
#'
#' @examples
preprocess_eye_events <- function(df_events, eyetracker){
  if(eyetracker == "SR 1000") return(SR1000.preprocess_eye_events(df_events))
  return(eyetracker_not_found(eyetracker))
}

#' Title
#'
#' @param df_fixations
#' @param eyetracker
#'
#' @return
#' @export
#'
#' @examples
preprocess_eye_fixations <- function(df_fixations, eyetracker){
  if(eyetracker == "SR 1000") return(SR1000.preprocess_eye_fixations(df_fixations))
  return(eyetracker_not_found(eyetracker))
}

#' Title
#'
#' @param df_fixations loaded table with fixations
#' @param original_resolution defined as a list with width and height in pixesls. Ex: list(width=1920, height=1080)
#' @param target_resolution defined as a list with width and height in pixesls. Ex: list(width=1920, height=1080)
#'
#' @return
#' @export
#'
#' @examples
change_resolution <- function(df_fixations, original_resolution, target_resolution){
  df_fixations$position_x <- round(df_fixations$position_x/original_resolution$width * target_resolution$width)
  df_fixations$position_y <- round(df_fixations$position_y/original_resolution$height * target_resolution$height)
  return(df_fixations)
}

#' Removes fixations out of disp_resolution boundary
#'
#' @param df_fixations fixation table
#' @param disp_resolution defined as a list with width and height in pixesls. Ex: list(width=1920, height=1080)
#'
#' @return
#' @export
#'
#' @examples
remove_outlier_fixations <- function(df_fixations, disp_resolution = list(width = 1920, height = 1080)){
  df_fixations <- df_fixations[(df_fixations$position_x < disp_resolution$width) & (df_fixations$position_y < disp_resolution$height), ]
  df_fixations <- df_fixations[df_fixations$position_x > 0 & df_fixations$position_y > 0, ]
  return(df_fixations)
}

#' Title
#'
#' @param dt_fixations fixations
#' @param areas list of area lists. Each area list is a list of X and y vectors of length 2. Ex: list(x=c(0,10), y=c(0,10))
#'
#' @return
#' @export
#'
#' @examples
add_screen_area_fixations = function(dt_fixations, areas){
  dt_fixations[, area := ""]
  for (area in areas){
    dt_fixations[is_between(pos_x, area$x[1], ar$x[2]) & is_between(pos_y, area$y[1], area$y[2]), area:= area$name]
  }
  return(dt_fixations)
}

