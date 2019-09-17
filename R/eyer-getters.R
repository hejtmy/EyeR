#' Filters data between start and end time
#'
#' @param obj object to filter
#' @param start start time
#' @param end end time
#' @param ...
#'
#' @return filtered object
#' @export
#'
#' @examples
filter_times <- function(obj, start, end, ...){
  UseMethod("filter_times")
}

#' Filters data between start and end time
#'
#' @param obj Eyer object to filted data from
#' @param times is a dataframe or a numeric(2) start and end to filter parts of the eyetracking. If it is set, start and end are ignored
#' @param data_fields list of data fields (in obj$data) to filter. If empty, all fields are filtered
#' @param raw_times if TRUE, uses obj$info$start_time to calculate times to filter.
#' Useful if the data have 0 based times since start, but we want to filter based on other values.
#' @param recalculate_times If true, recalculates times to begin at 0 again. Changes obj$info$start_time.
#'
#' @return object with filtered data.
#' @export
#'
#' @examples
filter_times.eyer <- function(obj, times, data_fields = c(), raw_times = FALSE, recalculate_times = FALSE){
  start <- times[1]
  start_original <- start #saves for the recalculation
  if(raw_times){
    times <- times - obj$info$start_time
  }
  if(recalculate_times){
    obj$info$start_time <- obj$info$start_time + start
    ### DOESN'T WORK!
  }
  if(length(data_fields) == 0) data_fields <- names(obj$data)
  for(field in data_fields){
    df <- obj$data[[field]]
    if(!is.data.frame(times)){
      obj$data[[field]] <-  df[df$time >= times[1] & df$time < times[2], ]
      next
    }
    df_filtered <- data.frame()
    for(i in 1:nrow(times)){
      df_filtered <- rbind(df_filtered, df[df$time >= times$start[i] & df$time < times$end[i], ])
    }
    obj$data[[field]] <- df_filtered
  }
  return(obj)
}
