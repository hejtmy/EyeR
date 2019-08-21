#' Saving eyer object into raw or parsed structure
#'
#' @param obj eyer object ot save
#' @param folder where to save the data. Defaults to workign dir
#' @param name name of the files to be saved
#' @param raw if true, then entire object will be saved as an R dataobject.
#' otherwise fields will be parsed into separate readable csv and json files
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
save_eyer <- function(obj, folder =".", name = "unnamed", raw = F, ...){
  # validate eyer
  # Save data fields
  for(field in names(obj$data)){
    df <- obj$data[[field]]
    name_df <- paste0(name, "_eyer_", field, ".csv")
    savepath <- file.path(folder, name_df)
    write.table(df, file = savepath, sep = ";", row.names = F)
  }
  #save evrything else as json
  savepath <- file.path(folder, paste0(name, "_eyer_info.json"))
  jsonlite::write_json(obj$info, savepath)
}
