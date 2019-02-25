#' Returns fixations with added information about to which evnet each fixation belongs
#'
#' the logic is that two quest events in eyetracker should be separated similarly as two quests in the quest log
#' if that is correct, we can accept the eyetracking logs and use them to extract fixations for each quest
#' Even in correct table there are two events that are off, as those are differences between two different sets administered
#'
#' @param dt_fixations fixations data.table. Needs to have posX and posY
#' @param areas vector of lists with name and vector of points
#'

create_areas <- function(height, width){
  #assigns height width
  HEIGHT <- 1080
  WIDTH <- 1920
  quater <- function(num){return(num/4)}
  third <- function(num){return(num/3)}
  around <- function(middle, deviation){return(c(middle - deviation, middle + deviation))}
  map_area <- list(name = "map", x = c(0, third(WIDTH)), y = c(0, third(HEIGHT)))
  center_area <- list(name = "center", x = around(WIDTH/2, third(WIDTH)/2),
                     y = around(HEIGHT/2, third(HEIGHT)/2))
  # left upper corner
  control_area <- list(name = "control" , x = c(0, third(WIDTH)),
                      y = c(HEIGHT, HEIGHT - third(HEIGHT)))

  quest_area <- list(name = "quest", x = c(WIDTH - quater(WIDTH), WIDTH),
                    y = c(HEIGHT - quater(HEIGHT)/2, HEIGHT))

  areas <- list(map_area, center_area, control_area, quest_area)
  return(areas)
}

