#' #' tSaveMetadataToDB
#' #' Writes captured  metadata to FlowTrace Database
#' #' @param outputFolder path where error file(if generated) should be written
#' #'
#' #' @return 
#' #' @export
#' #'
#' #' @examples 
#' #' tSaveMetadataToDB(outPutFolder = "./my_project_folder/")
#' tSaveMetadataToDB <- function(outputFolder = "./") {
#'   
#'   if(!captureMetadata()){return()}
#' 
#'   tryCatch(
#'     withCallingHandlers({
#'       dbCon <- getDBCon("keys.json")
#'       if (is.null(dbCon)) {
#'         logErrorMessage("No metadata written to DB due to missing DB Connection.")
#'         return("")
#'       }
#'       on.exit(dbDisconnect(dbCon))
#'       
#'       dbBegin(dbCon)
#' 
#'       for (si in tlvar$SYSTEM_INFO) {
#'         insertSystemInfoToDB(dbCon, si)
#'       }
#' 
#'       # ToDo: Do not write metadata to DB if there is no repo info....
#'       for (fi in tlvar$FILE_INFOS) {
#'         if (fi$repoType == "Local") {
#'           warning(paste0("FileInfo for local file written to DB: ",fi$localFilePath), call. = FALSE)
#'         }
#'         insertFileInfoToDB(dbCon, fi)
#'       }
#' 
#'       systemId <- getLastSystemId(dbCon)
#' 
#'       for (ai in tlvar$ACTION_INFOS) {
#'         ai$systemId <- systemId
#'         insertActionInfoToDB(dbCon, ai)
#'       }
#' 
#'       dbCommit(dbCon)
#'     },
#'     warning = function(w) { # necessary ???
#'       # just to display
#'       logWarning(w)
#'     },
#'     error = function(e) {
#'       dbRollback(dbCon)
#'       writeToLog(e)
#'     }
#'     ),
#'     error = function(e) {
#'       # writeToLog(e)
#'     }
#'   )
#' 
#'   # done in tEndMetadataCapture  -  only problem: 
#'   # if this function is called after tEndMetadataCapture, 
#'   # the SaveDB specific warnings are not written.
#'   # if (length(tlvar$ERROR_LOG) > 0) {
#'   #   writeErrorLogToJson(
#'   #     lapply(tlvar$ERROR_LOG, as.character),
#'   #     outputFolder
#'   #   )
#'   # }
#' }
#' 
#' 
#' insertFileInfoToDB <- function(dbCon, fi) {
#'   dbFields <- dbListFields(dbCon, c(tlconst$DB_SCHEMA, "file"))
#'   infosFrame <- fi$addToFrame()
#'   dataToInsert <- infosFrame[, which((names(infosFrame) %in% dbFields) == TRUE)]
#'   
#'   if (fileInfoPrimaryKeyExists(dbCon, fi$fileHash, fi$repoPath, fi$repoVersion) == FALSE) {
#'     # print(paste(fi$fileHash,fi$repoPath,fi$repoVersion,collapse = "-"))
#'     dbWriteTable(dbCon, c(tlconst$DB_SCHEMA, "file"),
#'                  value = dataToInsert, append = TRUE,
#'                  row.names = FALSE
#'     )
#'   } else {
#'     if (fi$accessInfo == "write") {
#'       warning(paste0( # Change this to error/write warnings to log file
#'         "Could not overwrite file meta data for ", fi$localFilePath,
#'         " for action ", fi$actionInfo, " as the file is unchanged"
#'       ))
#'     }
#'   }
#' }
#' 
#' insertActionInfoToDB <- function(dbCon, ai) {
#'   # error handling done in tSaveMetadataToDB
#'   actionFrame <- ai$toActionFrame(dbListFields(dbCon, c(tlconst$DB_SCHEMA, "action")))
#'   
#'   dbWriteTable(dbCon, c(tlconst$DB_SCHEMA, "action"), value = actionFrame, append = TRUE, row.names = FALSE)
#'   ai$actionId <- getLastActionId(dbCon) 
#'   
#'   actionFileFrame <- ai$toActionFileFrame(dbListFields(dbCon, c(tlconst$DB_SCHEMA, "action_file")))
#'   
#'   dbWriteTable(dbCon, c(tlconst$DB_SCHEMA, "action_file"), value = actionFileFrame, append = TRUE, row.names = FALSE)
#' }
#' 
#' insertSystemInfoToDB <- function(dbCon, si) {
#'   dbFields <- dbListFields(dbCon, c(tlconst$DB_SCHEMA, "system"))
#'   infosFrame <- si$addToFrame()
#'   dataToInsert <- infosFrame[, which((names(infosFrame) %in% dbFields) == TRUE)]
#'   
#'   dbWriteTable(dbCon, c(tlconst$DB_SCHEMA, "system"),
#'                value = dataToInsert, append = TRUE,
#'                row.names = FALSE
#'   )
#' }
#' 

