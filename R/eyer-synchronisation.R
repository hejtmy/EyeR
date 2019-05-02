#' tries to find a sequency of N elements in eye_durations that correspond to the synchro durations
#' returns index of first matchin eye event
#'
#' @param eye_durations
#' @param set_synchro_durations
#' @param allowed_difference
#'
find_group_match = function(eye_durations, set_synchro_durations, allowed_difference){
  idx_table = data.table(eye_idx = numeric(), synchro_idx = numeric())
  #finds the first closest idx after the last found index
  n_eye = length(eye_durations)
  n_synchro = length(set_synchro_durations)
  #VALIDATE

  sum_diff_synchro = sum(set_synchro_durations)
  best_start_value = allowed_difference * n_synchro #this initialisation insures the first assignments
  best_start_idx = NULL
  #logically this should be n_synchro - 1, but we have NA value in eye duration at the end, so we iterate only n_times
  for (eye_start_id in 1:(n_eye - n_synchro)){
    sel_eye_durations = eye_durations[eye_start_id:(eye_start_id + n_synchro - 1)]
    sum_diff_eye = abs(sum_diff_synchro - sum(sel_eye_durations))
    #if sum of differences in these elements it combined lesser than allowed difference per element, we note it
    if(sum_diff_eye < allowed_difference * n_synchro){
      # but only if its better than the already best one
      if (sum_diff_eye < best_start_value) best_start_idx = eye_start_id
    }
  }
  #if we found such a match, we note it as
  #TODO - maybe check again
  return(best_start_idx)
}

#' Returns either a list with best synchronised event or NULL if none is found
#'
#' @param eye_durations time separation between each same eye event in milliseconds
#' @param unity_durations time separation between unity events in one set
#' @param allowed_difference allowed difference between times to be still considered similar
find_better_match = function(eye_durations, unity_durations, allowed_difference, consecutive){
  matching = list(unity = NA, eye = NA, diff = NA)
  n_matches = 0
  for (i in 1:length(unity_durations)){
    dur = unity_durations[i]
    if(is.na(dur)) next
    id = which(abs(eye_durations - dur) < allowed_difference)
    if (length(id) == 1){
      #We try to figure out, if the consecutive synchro points also match
      if (has_consecutive(unity_durations[i:length(unity_durations)], eye_durations[id:length(eye_durations)], allowed_difference, consecutive)){
        n_matches = consecutive + 1
        matching$unity = i
        matching$eye = id
        matching$diff = dur
        break
      }
    }
  }
  if (n_matches == 0){
    SmartPrint(c("WARNING:find_better_match:NoMatch", "DESCRIPTION: No matching events found. Synchronising could not be finished"))
    return(NULL)
  }
  matching$n_matches = n_matches
  return(matching)
}


#' replaces eyetracker_times for quest_times
#'
#' @param df_sync_times
#' @param fixations
eye_to_unity_times = function(fixations, df_sync_times){
  df_sync_times = mutate(df_sync_times, time_diff = (time_unity * 1000) - time_eye)
  #' - this loop might seems weird, as it always modifies all the times regardless of the set
  #' but it actually rewrites only portions of sequencial sets - rewrites 1st with 2nd and 1st and 2nd with 3rd
  fixations[, `:=`(start_unity = as.numeric(NA),
                   end_unity = as.numeric(NA))]
  for (i in 1:nrow(df_sync_times)){
    row = df_sync_times[i, ]
    fixations[start >  row$time_eye & end > row$time_eye, `:=`(start_unity = (start + row$time_diff)/1000,
                                                               end_unity = (end + row$time_diff)/1000)]
  }
  return(fixations)
}

#' This function decides whether the list is acceptable
#'
#' @param ls list with follwing parameters diff - time duration of the event separation, n_matches - how many good matches were found
eye_synchro_acceptable = function(ls){
  MINUTE_MS = 60 * 1000
  MIN_MATCHES = 2
  #if (ls$diff < MINUTE_MS) return(FALSE)
  if (ls$n_matches < MIN_MATCHES){
    SmartPrint(c("WARNING:synchronise_eye_unity:NotEnoughData", "DESCRIPTION: Synchronising only based on a single event"))
    return(TRUE)
  }
  # tell it went alright
  return(TRUE)
}


#' Returns a data frome with synchronising times for each set
#' Table is in format of set_id, time_eyetracker, time_unity
#' @param eye_event Name of the event in the eyetracker log
#' @param unity_event Name of the event in the unity log
#' @param eye_times All eyetrackder event times
#' @param unity_times All unity event times
#' @param allowed_difference How far can two events be separated to be still considered comming from the same source in milliseconds

try_fit_event = function(eye_event, unity_event, eye_times, unity_times, allowed_difference, consecutive = 3){
  eye_events = copy(eye_times)
  eye_events = eye_events[type == eye_event, ]
  eye_events[, diff:= c(NA, diff(time))]

  ## NEEDS CHECKING becasue of index number
  eye_durations = shift(eye_events$diff, 1, type = "lead") #differences between two eye event times in the eyetracker log
  unity_durations = unity_times[Input == unity_event, .(Time, duration_ms = c(diff(Time) * 1000, NA)), by = set_id]

  set_ids = unique(unity_durations[, set_id])
  n_sets = length(set_ids) #it is possible that one proband can have only 2 or one set

  #preparing the data frame output
  df = data.frame(set_id = unique(unity_durations[, set_id]), time_eye = rep(NA, n_sets), time_unity = rep(NA, n_sets))

  for (data_set_id in set_ids){

    unity_set_durations = unity_durations[set_id == data_set_id]

    # this passes the set durations from unity and all durations from eyetracker to the function that shoudl find best match
    # find_better_match returns a list with the best synchronising event
    ls_idx = find_better_match(eye_durations, unity_set_durations[, duration_ms], allowed_difference, consecutive)

    #if the set is accepted and it is the accepted by the defined accepting function, we fill the return df with synchronising times
    if(!is.null(ls_idx) && eye_synchro_acceptable(ls_idx)){
      df[df$set_id == data_set_id, ]$time_eye = eye_events[ls_idx$eye]$time
      df[df$set_id == data_set_id, ]$time_unity = unity_set_durations[ls_idx$unity, Time]
    }
  }
  return(df)
}
