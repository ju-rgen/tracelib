#' tSaveMetadataToFile
#' Saves captured metadata information to a json file
#' @param outputFolder directory path where file should be written, defaults to current working directory if not provided
#' @param jsonFileName filename
#' @param useTimeStamp timestamp as suffix to filename
#'
#' @return json metadata file
#' @export
#'
#' @examples 
#' tSaveMetadataToFile(outputFolder="~/my_working_directory/", jsonFileName="my_project",useTimeStamp=T)
tSaveMetadataToFile <- function(outputFolder = "./", jsonFileName = "", useTimeStamp = T) {
  
  if(!captureMetadata()){return()}
  
  tryCatch(
    withCallingHandlers({
      writeInfosToJson(
        fileInfos = tlvar$FILE_INFOS,
        actionInfos = tlvar$ACTION_INFOS,
        systemInfo = tlvar$SYSTEM_INFO,
        outputFolder = outputFolder,
        jsonFileName = jsonFileName,
        useTimeStamp = useTimeStamp
      )
    },
    error = function(e) {
      writeToLog(e)
    }
    ),
    error = function(e) {
      # writeToLog(e)
    }
  )

  # done in tEndMetadataCapture 
  # if (length(tlvar$ERROR_LOG) > 0) {
  #   writeErrorLogToJson(
  #     lapply(tlvar$ERROR_LOG, as.character),
  #     outputFolder
  #   )
  # }
}
