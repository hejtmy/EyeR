
###
SR1000.read_eye_fixations <- function(text){
  FIX_idxs <- grep('^EFIX.*', text)
  lines <- text[FIX_idxs]

  #Replaces all the EFIX R/L part up to the number
  lines <- gsub('^EFIX R\\s+', '', lines, perl=T)
  lines <- gsub('^EFIX L\\s+', '', lines, perl=T)

  #creates one file with each char on a single line
  text <- paste(lines, sep="", collapse="\n")
  tab <- fread(text, sep = "\t", header = F)
  colnames(tab) <- c("start", "end", "no_idea_1", "pos_x", "pos_y", "no_idea_2")
  return(tab)
}

SR1000.read_eye_events <- function(text){
  text <- readLines(filepath)
  EVENT_NAMES = c("KEY_UP", "KEY_DOWN")
  MSG_indices = grep('^MSG\\t+.*', text)
  lines = text[MSG_indices]
  event_indices = sapply(lines, contains_word, EVENT_NAMES)
  lines = lines[event_indices]
  #removing the MSG part
  lines = gsub('^MSG\t', '', lines, perl = T)
  #creates one file with each char on a single line
  text = paste(lines, sep = "", collapse = "\n")
  tab = fread(text, sep = " ", header = F)
  tab[,c("V2", "V4", "V5", "V6"):= NULL]
  colnames(tab) = c("time", "name", "type")
  return(tab)
}

SR1000.read_eye_movements = function(text){
  DATA_indexes = grep("^[0-9]+.*$", text)
  pseudo_file = paste(text[DATA_indexes], collapse="\n")

  dat = fread(pseudo_file, header = F, col.names = c("Frame", "X", "Y", "Pupil", "NoIdea", "SomeDots"))
  dat[, X := as.double(X)]
  dat[, Y := as.double(Y)]
  return(dat)
}
