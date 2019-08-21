#' Changes object resolution
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
change_resolution <- function(obj, original_resolution, target_resolution){
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

