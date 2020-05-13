#' tStartMetadataCapture
#' Starts meta data capture, Initiates global objects for file, action and system infos
#'
#' @param repoPath optional: path of folder or file in repository to determine the PMx activity
#' @param filePath optional: local path of folder or file in checkout folder to determine the PMx activity
#' @param actionType should be one of Run, DataImport, TLFGeneration, ReportGeneration, Edit, Move, Copy, AnalysisFileGeneration, Analysis or Other
#' @param offset optional: if action should be defined by a calling function of higher level
#'
#' @return
#' @export
#'
#' @examples
#' tStartMetadataCapture(metaDataCapture = T)
tStartMetadataCapture <- function(metaDataCapture = T, repoPath = "", filePath = "", actionType = "Run", offset = 0) {
  # Integration tests

  tlvar$METADATA_CAPTURE <- metaDataCapture
  
  if(!captureMetadata()){return()}
  
  tlvar$ERROR_LOG <- list()
  tlvar$ERROR_LOG_MODE <- "Standard" # ("Standard"|"Extended")
  
  tryCatch(withCallingHandlers({
    
    initConst()
    
    tlvar$FILE_INFOS <- list()
    tlvar$ACTION_INFOS <- list()
    tlvar$ACTION_STACK <- list()
    tlvar$SYSTEM_INFO <- list()
    
    si <- SystemInfo()
    tlvar$SYSTEM_INFO[[si$systemId]] <- si
    
    tlvar$ACTIVITY_ID <- ""
    tStartAction(actionType = actionType, offset = offset + 1) # open top level action and remove row call of this function itself
    
    # get and set activityId
    activeAction <- getActiveAction()
    
    # JJ 2020-04-03 separate DB code - see comment for determineActivityId in db_query.R
    # tlvar$ACTIVITY_ID <- determineActivityId(repoPath = repoPath, filePath = filePath)
    # activeAction$activityId <- tlvar$ACTIVITY_ID
    
  }, error = function(e) {
    writeToLog(e)
  }), error = function(e) {
    e
  })
}

#' tEndMetadataCapture
#' Ends metadata capture and saves metadata to File or DB as specified by user
#'
#' @param outputFolder where to store error logfile, json File
#' @param jsonFileName name of metadata output file (only for storageMode = "File")
#' @param useTimeStamp whether timestamp should be used as filename suffix (only for storageMode = "File")
#'
#' @return
#' @export
#'
#' @examples
#' tEndMetadataCapture(storageMode = "File", outputFolder="~/my_working_directory/", jsonFileName="my_project",useTimeStamp=T)
#' tEndMetadataCapture(storageMode = "DB", outputFolder="~/my_working_directory/")
tEndMetadataCapture <- function(storageMode = "File", outputFolder = "./", jsonFileName = "", useTimeStamp = T, offset = 0) {
  # JJ 2020-04-03 separate DB code
  # argument storageMode = c("DB","File") changed, kept just for compatibility reasons in tests
  # @param storageMode should be one of "None", "File" or "DB"
  
  if(!captureMetadata()){return()}
  
  tryCatch(withCallingHandlers({
    # storageMode <- match.arg(storageMode) # check, that access is one of these values
    
    # update empty activityIds of actions and written files 
    # in case an activityId could not be determined in tStartMetadataCapture
    # and was first determined by an output file in the meantime, see tStoreFileMetadata
    if (!is.null(tlvar$ACTIVITY_ID) && tlvar$ACTIVITY_ID != "" && getActiveAction()$activityId == "" )
    {
      for (ai in tlvar$ACTION_INFOS) {
        if (!is.null(ai$activityId) && ai$activityId == "") {
          ai$activityId <- tlvar$ACTIVITY_ID 
          for (fi in ai$outputFileInfos) {
            if (!is.null(fi$accessInfo) && fi$accessInfo == "write" && (is.null(fi$activityId) || fi$activityId == "")) { fi$activityId <- tlvar$ACTIVITY_ID }
          }
        }
      }
      # alternative for output files
      # for (fi in tlvar$FILE_INFOS) {
      #   if (fi$accessInfo == "write" & fi$activityId == "") { fi$activityId <- tlvar$ACTIVITY_ID }
      # }
    }

    tEndAction(offset = offset + 1) # close top level action
    tSaveMetadataToFile(outputFolder = outputFolder, jsonFileName = jsonFileName, useTimeStamp = useTimeStamp)

    #  JJ 2020-04-03 separate DB code
    # if (storageMode == "DB"){
    #   tSaveMetadataToDB(outputFolder = outputFolder)
    # }
    # else if (storageMode == "File"){
    #   tSaveMetadataToFile(outputFolder = outputFolder, jsonFileName = jsonFileName, useTimeStamp = useTimeStamp)
    # }
    
    if (length(tlvar$ERROR_LOG) > 0) {
      writeErrorLogToJson(
        lapply(tlvar$ERROR_LOG, as.character),
        outputFolder
      )
    }
  }, error = function(e) {
    writeToLog(e)
  }), error = function(e) {
    e
  })
}


