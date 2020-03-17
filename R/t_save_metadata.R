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

#' tSaveMetadataToDB
#' Writes captured  metadata to FlowTrace Database
#' @param outputFolder path where error file(if generated) should be written
#'
#' @return 
#' @export
#'
#' @examples 
#' tSaveMetadataToDB(outPutFolder = "./my_project_folder/")
tSaveMetadataToDB <- function(outputFolder = "./") {
  
  if(!captureMetadata()){return()}

  tryCatch(
    withCallingHandlers({
      dbCon <- getDBCon("keys.json")
      if (is.null(dbCon)) {
        logErrorMessage("No metadata written to DB due to missing DB Connection.")
        return("")
      }
      on.exit(dbDisconnect(dbCon))
      
      dbBegin(dbCon)

      for (si in tlvar$SYSTEM_INFO) {
        si$insertToDB(dbCon)
      }

      # ToDo: Do not write metadata to DB if there is no repo info....
      for (fi in tlvar$FILE_INFOS) {
        if (fi$repoType == "Local") {
          warning(paste0("FileInfo for local file written to DB: ",fi$localFilePath), call. = FALSE)
        }
        fi$insertToDB(dbCon)
      }

      systemId <- getLastSystemId(dbCon)

      for (ai in tlvar$ACTION_INFOS) {
        ai$systemId <- systemId
        ai$insertToDB(dbCon)
      }

      dbCommit(dbCon)
    },
    warning = function(w) { # necessary ???
      # just to display
      logWarning(w)
    },
    error = function(e) {
      dbRollback(dbCon)
      writeToLog(e)
    }
    ),
    error = function(e) {
      # writeToLog(e)
    }
  )

  # done in tEndMetadataCapture  -  only problem: 
  # if this function is called after tEndMetadataCapture, 
  # the SaveDB specific warnings are not written.
  # if (length(tlvar$ERROR_LOG) > 0) {
  #   writeErrorLogToJson(
  #     lapply(tlvar$ERROR_LOG, as.character),
  #     outputFolder
  #   )
  # }
}


#' #' Writes the metadata to DB or File based on user input
#' #'
#' #' @param jsonFilePath If storage mode is JSON, provide file path
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #' tEndRun()
#' #' tEndRun(jsonFilePath = "../../out.json")
#' tSaveMetaData <- function(jsonFilePath = "./", jsonFileName = "", useTimeStamp = T) { # Default jsonfilepath is working directory if not specified
#'   #Integration test
#'   if (!tlvar$METADATA_CAPTURE) {
#'     return()
#'   }
#'   tryCatch(
#'     withCallingHandlers({
#'       tEndAction() # close top level action
#'       # These DB writing steps are prone to error. Needs detailed error catching
#'       if (STORAGE_MODE == "DB") {
#'
#'         for (si in tlvar$SYSTEM_INFO) {
#'           # insertInfosToDb(si$addToFrame(), DBCON, "system")
#'           si$insertToDB(tlvar$DBCON)
#'         }
#'
#'         # ToDo: Do not write metadata to DB if there is no repo info....
#'         for (fi in tlvar$FILE_INFOS) {
#'           # insertInfosToDb(fi$addToFrame(), DBCON, "file")
#'           fi$insertToDB(tlvar$DBCON)
#'         }
#'
#'         systemId <- getLastSystemId(tlvar$DBCON)
#'
#'         logCallStack(prefix = "endRun1_")
#'
#'
#'         for (ai in tlvar$ACTION_INFOS) {
#'           ai$systemId <- systemId
#'           # frames <- ai$addToFrame(actionFrame, actionFileFrame)
#'           ai$insertToDB(tlvar$DBCON)
#'           #ai$insertActionFilesToDB(DBCON)
#'           # insertInfosToDb(frames$actionFileFrame, DBCON, "action_file")
#'         }
#'       } else if (STORAGE_MODE == "File") {
#'         writeInfosToJson(
#'           fileInfos = tlvar$FILE_INFOS,
#'           actionInfos = tlvar$ACTION_INFOS,
#'           systemInfo = tlvar$SYSTEM_INFO,
#'           jsonFilePath = jsonFilePath,
#'           jsonFileName = jsonFileName,
#'           useTimeStamp = useTimeStamp
#'         )
#'       }
#'     },
#'     error = function(e) {
#'       writeToLog(e)
#'     }
#'     ),
#'     error = function(e) {
#'       #writeToLog(e)
#'     }
#'   )
#'   if (!is.null(tlvar$DBCON)){
#'     dbDisconnect(tlvar$DBCON)
#'   }
#'
#'   if (length(tlvar$ERROR_LOG) > 0) {
#'     writeErrorLogToJson(
#'       lapply(tlvar$ERROR_LOG, as.character),
#'       jsonFilePath
#'     )
#'   }
#'   # Check if gActionStack is empty and if not, generate warning
#'   # Include length(gActionStack) and function name of last element in the warning
#' }
