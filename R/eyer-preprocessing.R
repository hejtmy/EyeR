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
  obj$info$resolution <- target
  return(obj)
}

#' @export
change_resolution.data.frame <- function(df, original, target){
  #validations and check for errors
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

#' Flips the given axis to its negative
#'
#' @details this function flips given axis (x or y) anchored to the anchor value.
#'
#' @description Some eyetrackers output its data with inversed values. E.g. Eyelink returns data with
#' 0,0 in the left top corner, but visualisations make more sense with 0,0 being projeccted to the
#' left bottom corner. So we neeed to "flip" the Y axis. But we also need to define the new "anchor".
#' In our case, if we want the current 0,0 to become left top corner, we want to reanchor the "y" axis with 0
#' to be at current height (e.g. 1080).
#'
#' @param obj \code\link{EyerObject}}
#' @param axis string of which axis to flip. c("x", "y")
#' @param anchor what is the value of new 0? Needs to be deffined
#' @return
#' @export
#'
#' @examples
flip_axis <- function(obj, axis, anchor){
  if(!is.eyer(obj)){
    warning("passed object is not eyer")
    return(NULL)
  }
  if(!(axis %in% c("y", "x"))){
    warning("can only flip x and y axes")
    return(NULL)
  }
  for(position_data in EYE_POSITION_DATA_FIELDS){
    df <- obj$data[[position_data]]
    if(is.null(df)) next
    df[[axis]] <- anchor - df[[axis]]
    obj$data[[position_data]] <- df
  }
  return(obj)
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
#' @param obj \code\link{EyerObject}}
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
  if(nrow(df) < 1) return(df)
  df <- df[seq(1, nrow(df), n), ]
  return(df)
}

#' Add area column to object data
#'
#' @details Adds new column to the fixations and gaze data frame from the area
#'
#' @param obj \code\link{EyerObject}}
#' @param areas list or a vector of \code\link{AreaObject}}
#'
#' @return modified \code\link{EyerObject}} or the object back if something doesn't work
#' @export
#'
#' @examples
add_area_column <- function(obj, areas){
  if(!is.eyer(obj)){
    warning("passed object is not eyer")
    return(obj)
  }
  for(eye_data_field in EYE_POSITION_DATA_FIELDS){
    df <- obj$data[[eye_data_field]]
    if(is.null(df)) next
    df$area <- ""
    for (area in areas){
      if(!is_valid_area(area)) next
      in_area <- is_in_area(df$x, df$y, area)
      df$area[in_area] <- area$name
    }
    obj$data[[eye_data_field]] <- df
  }
  return(obj)
}

is_in_area <- function(x, y, area){
  if(area$type == "square"){
    return(is_in_area_square(x, y, area$points))
  }
}

is_in_area_square <- function(x, y, points){
  return((x >= points$xmin & x <= points$xmax) & (y >= points$ymin & y <= points$ymax))
}
