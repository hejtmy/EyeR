#' Saving eyer object into raw or parsed structure
#'
#' @param obj eyer object ot save
#' @param folder where to save the data. Defaults to working dir. Tries to create the folder if it doesn't exist
#' @param name name of the files to be saved
#' @param robject if true, then entire object will be saved as an R dataobject.
#' otherwise fields will be parsed into separate readable csv and json files
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
save_eyer <- function(obj, folder =".", name = "unnamed", robject = FALSE, ...){
  # validate eyer
  # check for folder and create if it doesn't esits
  if(!dir.exists(folder)) dir.create(folder)
  # Save data fields
  for(field in names(obj$data)){
    df <- obj$data[[field]]
    if(nrow(df) < 1) next
    name_df <- paste0(name, "_eyer_", field, ".csv")
    savepath <- file.path(folder, name_df)
    write.table(df, file = savepath, sep = ";", row.names = FALSE)
  }
  #save evrything else as json
  savepath <- file.path(folder, paste0(name, "_eyer_info.json"))
  jsonlite::write_json(obj$info, savepath)
}
