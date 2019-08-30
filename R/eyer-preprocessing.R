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

#'  Removes points out of resolution boundary
#'
#' @param obj OBject
#' @param replace what should the out of bounds values be replaced with? if NULL, rows are delted. NULL by default
#' @param resolution efined as a list with width and height in pixesls. e.g: list(width=1920, height=1080)
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
remove_out_of_bounds <- function(obj, replace = NULL, resolution = NULL, ...){
  UseMethod("remove_out_of_bounds")
}

#'  Removes points out of resolution boundary
#'
#' @param obj OBject
#' @param replace what should the out of bounds values be replaced with? if NULL, rows are delted. NULL by default
#' @param resolution efined as a list with width and height in pixesls. e.g: list(width = 1920, height = 1080)
#'
#' @return
#' @export
#'
#' @examples
remove_out_of_bounds.eyer <- function(obj, replace, resolution){
  if(is.null(resolution)) resolution <- obj$info$resolution
  if(is.null(resolution)){
    warning("Object doesn't have resolution value in info and no resulotion passed, returning unmodified object")
    return(obj)
  }
  if(nrow(obj$data$fixations) > 0) obj$data$fixations <- remove_out_of_bounds.data.frame(obj$data$fixations, replace, resolution)
  if(nrow(obj$data$gaze) > 0) obj$data$gaze <- remove_out_of_bounds.eyer(obj$data$gaze, original, target)
  return(obj)
}

#' Removes points out of resolution boundary
#'
#' @param df table with x, y, columns
#' @param replace what should the out of bounds values be replaced with? if NULL, rows are delted. NULL by default
#' @param resolution defined as a list with width and height in pixesls. e.g: list(width=1920, height=1080)
#'
#' @return eyer object with replaced falues in gaze and fixations
#' @export
#'
#' @examples
remove_out_of_bounds.data.frame <- function(df, replace, resolution){
  if(is.null(replace)) {
    df <- df[df$x < df$width & df$y < df$height, ]
    df <- df[df$x > 0 & df$y > 0, ]
  } else {
    df[df$x < df$width & df$y < df$height, c("x", "y")] <- c(replace, replace)
    df[df$x > 0 & df$y > 0, c("x", "y")]  <- c(replace, replace)
  }
  return(df)
}

#' Downsamples data
#'
#' @param obj object to downsample
#' @param n picks every nth recording
#' @param ...
#'
#' @return donwsampled object
#' @export
#'
#' @examples
downsample <- function(obj, n, ...){
  UseMethod("downsample")
}

#' Downsamples eyer gaze data
#'
#' @param obj EyerOBject
#' @param n picks every nth recording
#' @param ...
#'
#' @return EyerObject with downsampled gaze data
#' @export
#'
#' @examples
downsample.eyer <- function(obj, n, ...){
  obj$data$gaze <- downsample.data.frame(obj$data$gaze, n)
  return(obj)
}


#' @export
downsample.data.frame <- function(df, n){
  df <- df[seq(1, nrow(df), n), ]
  return(df)
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

