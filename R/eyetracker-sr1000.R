# SR 1000 Eyelink eyetracker functions


SR1000.read_eye_fixations <- function(text){
  FIX_idxs <- grep('^EFIX.*', text)
  lines <- text[FIX_idxs]

  #Replaces all the EFIX R/L part up to the number
  lines <- gsub('^EFIX R\\s+', '', lines, perl=T)
  lines <- gsub('^EFIX L\\s+', '', lines, perl=T)

  #creates one file with each char on a single line
  text <- paste(lines, sep="", collapse="\n")
  tab <- read.table(text = text, sep = "\t", header = F)
  colnames(tab) <- c("start", "end", "no_idea_1", "position_x", "position_y", "no_idea_2")
  return(tab)
}

SR1000.read_eye_events <- function(text){
  EVENT_NAMES <- c("KEY_UP", "KEY_DOWN")
  i_msg <- grep('^MSG\\t+.*', text)
  lines <- text[i_msg]
  i_events <- sapply(lines, contains_word, EVENT_NAMES)
  lines <- lines[i_events]
  #removing the MSG part
  lines = gsub('^MSG\t', '', lines, perl = T)
  #creates one file with each char on a single line
  text <- paste(lines, sep = "", collapse = "\n")
  tab <- read.table(text = text, sep = " ", header = F)
  tab[,c("V2", "V4", "V5", "V6")] <- NULL
  colnames(tab) <- c("time", "name", "type")
  return(tab)
}

SR1000.read_eye_movements = function(text){
  DATA_indexes <- grep("^[0-9]+.*$", text)
  pseudo_file <- paste(text[DATA_indexes], collapse="\n")
  dat <- read.table(text = pseudo_file, header = F, col.names = c("Frame", "position_x", "position_y", "Pupil", "NoIdea", "SomeDots"))
  dat$position_x <- as.double(dat$position_x)
  dat$position_y <- as.double(dat$position_y)
  return(dat)
}
