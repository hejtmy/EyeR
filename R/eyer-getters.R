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
#' @param start start time (inclusive) as is in the time column
#' @param end end time (exclusive) as is in the time column
#' @param data_fields list of data fields (in obj$data) to filter. If empty, all fields are filtered
#' @param raw_times if T, uses obj$info$start_time to calculate times to filter.
#' Useful if the data have 0 based times since start, but we want to filter based on other values. **default** is FALSE
#' @param recalculate_times If true, recalculates times to begin at 0 again. Changes obj$info$start_time. **default** is FALSE
#'
#' @return object with filtered data.
#' @export
#'
#' @examples
filter_times.eyer <- function(obj, start, end, data_fields = c(), raw_times = F, recalculate_times = F){
  start_original <- start #saves for the recalculation
  if(raw_times){
    start <- start - obj$info$start_time
    end <- end - obj$info$start_time
  }
  if(recalculate_times){
    obj$info$start_time <- obj$info$start_time + start
  }
  if(length(data_fields) == 0) data_fields <- names(obj$data)
  for(field in data_fields){
    df <- obj$data[[field]]
    obj$data[[field]] <-  df[df$time >= start & df$time < end, ]
  }
  return(obj)
}
