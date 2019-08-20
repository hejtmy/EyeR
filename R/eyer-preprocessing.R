#' Reads and cleans events
#'
#' @param df_events data frame with events loaded
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

#' Preprocesses loaded fixations. This usually means adding columns or recalculating
#' values that are not not well read
#'
#' @param df_fixations data.frame with fixations loaded
#' @param eyetracker which eyetracker was used. See readme for more information.
#'
#' @return dataframe with generic preprocessing defined for given eyetracker
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
#' @param original_resolution defined as a list with width and height in pixesls.
#' Ex: list(width=1920, height=1080)
#' @param target_resolution defined as a list with width and height in pixesls.
#' Ex: list(width=1920, height=1080)
#'
#' @return data frame with fixations with changed resolution. Can still yield out of bounds
#' @export
#'
#' @examples
change_resolution <- function(df_fixations, original_resolution, target_resolution){
  df_fixations$x <- round(df_fixations$x/original_resolution$width * target_resolution$width)
  df_fixations$y <- round(df_fixations$y/original_resolution$height * target_resolution$height)
  return(df_fixations)
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

