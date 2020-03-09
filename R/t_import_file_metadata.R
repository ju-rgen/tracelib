#' creates action for the import of one file from source to target location
#' @description besides a description for the action only metadata for the source and the target file and location are passed.
#'
#' @param importActionDescription optional: any identifying and/or valuable information about the import action
#' @param sourceRepoType optional: should contain information about the type of the source repository, if available
#' @param sourceFilePath path including the source file name; just the source file name, if no path is available
#' @param sourceFileHash md5 file hash of source file 
#' @param sourceRepoVersion optional: version of source file in source repository, if available
#' @param sourceRepoRoot optional: root path of the source repository, if available
#' @param sourceModified optional: timestamp of last change of source file, if available
#' @param sourceModifiedBy optional: username or name or similar of provider of source file
#' @param sourceQualityChecked flag, if the source file is confirmed by the provider to be ready for usage in GxP workflows
#' @param sourcedescription optional: any valuable additional information available for the source file
#' @param targetRepoType information about the type of the target repository
#' @param targetFilePath path including the target file name in the repository, e.g. "https://svnserver/svn/reponame/trunk/folder/file.csv"
#' @param targetFileHash md5 file hash of target file 
#' @param targetRepoVersion version of the checked in target file in the target version control system, e.g. svn
#' @param targetRepoRoot repository path, e.g. "https://svnserver/svn/reponame"
#' @param targetModified check in timestamp in target repository, e.g. available via svninfo 
#' @param targetModifiedBy user name or name of importing person
#' @param targetQualityChecked flag, if the target file is confirmed by the provider or by the importing person to be ready for usage in GxP workflows
#' @param targetdescription optional: any valuable additional information available for the target file
#' @param offset should be overridden only in special cases
#' 
#' @return
#' @export
#'
#' @examples
tImportFile <-
  function(importActionDescription = "",

             sourceRepoType = "Unknown",
             sourceFilePath = "Unknown",
             sourceFileHash = "Unknown",
             sourceRepoVersion = "Unknown",
             sourceRepoRoot = "",
             sourceModified = tlconst$OT,
             sourceModifiedBy = "",
             sourceQualityChecked = F,
             sourceDescription = "",

             targetRepoType = "Unknown",
             targetFilePath = "Unknown",
             targetFileHash = "Unknown",
             targetRepoVersion = "Unknown",
             targetRepoRoot = "",
             targetModified = tlconst$OT,
             targetModifiedBy = "",
             targetQualityChecked = F,
             targetDescription = "",

             offset = 0) {
    # Integration test
    if(!captureMetadata()){return()}

    tryCatch(
      withCallingHandlers({
  
        tStartAction(actionType = "DataImport", offset = offset + 1) # create action to keep source/target relationship

        fiSource <- FileInfo$new(init = "manual", access = "read")
        fiTarget <- FileInfo$new(init = "manual", access = "write")

        activityId <- determineActivityId(repoPath = targetFilePath)
        if (activityId == "") {
          activityId <- tlvar$ACTIVITY_ID
        }

        fiSource$repoType <- sourceRepoType
        fiSource$repoPath <- sourceFilePath
        fiSource$repoVersion <- sourceRepoVersion
        fiSource$repoRoot <- sourceRepoRoot
        fiSource$fileType <- getFileType(sourceFilePath)
        fiSource$fileHash <- sourceFileHash
        fiSource$repoModified <- sourceModified
        fiSource$repoModifiedBy <- sourceModifiedBy
        fiSource$qualityChecked <- sourceQualityChecked
        fiSource$description <- sourceDescription
        fiSource$activityId <- activityId

        fiTarget$repoType <- targetRepoType
        fiTarget$repoPath <- targetFilePath
        fiTarget$repoVersion <- targetRepoVersion
        fiTarget$repoRoot <- targetRepoRoot
        fiTarget$fileType <- getFileType(targetFilePath)
        fiTarget$fileHash <- targetFileHash
        fiTarget$repoModified <- targetModified
        fiTarget$repoModifiedBy <- targetModifiedBy
        fiTarget$qualityChecked <- targetQualityChecked
        fiTarget$description <- targetDescription
        fiTarget$activityId <- activityId

        actionInfo <- getActiveAction()
        actionInfo$description <- importActionDescription
        actionInfo$activityId <- activityId

        fiTarget$actionInfo <- paste(actionInfo$actionId, actionInfo$actionName, collapse = "-")
        
        actionInfo$addInput(fiSource)
        actionInfo$addOutput(fiTarget)

        tlvar$FILE_INFOS[[fiSource$getKey()]] <- fiSource
        tlvar$FILE_INFOS[[fiTarget$getKey()]] <- fiTarget
      }, error = function(e) {
        writeToLog(e)
      }), error = function(e) {
        e
      })
    
    tEndAction()
    return("")
  }

#' creates dataframe for explicit metadata for files
#'
#' @return dataframe with columns "access","filePath","fileHash","repoType","repoRoot","repoPath","repoVersion","repoModified","repoModifiedBy","qualityChecked","description"
#' @export
#'
#' @examples
createFilesMetadataFrame <- function(){
  # wrong datatype for repoModified
  # colNames <- c("access","filePath","fileHash","repoType","repoRoot","repoPath","repoVersion","repoModified","repoModifiedBy","qualityChecked","description")
  # mdf <- setNames(data.frame( matrix(ncol = length(colNames), nrow = 0), stringsAsFactors = FALSE), colNames) 
  mdf <- data.frame(access = character(), filePath = character(), fileHash = character(), 
                    repoType = character(), repoRoot = character(), repoPath = character(),  repoVersion = character(), 
                    repoModified = as.POSIXct(character()), repoModifiedBy = character(), 
                    qualityChecked = logical(), description = character(), stringsAsFactors = FALSE)
  return(mdf)
}

#' appends row to dataframe from a named list
#'
#' @param dataframe
#' @param namedList
#'
#' @return dataframe with additional row with elements assigned to columns by names
#' @export
#'
#' @examples
addRow <- function(dataframe,namedList){
    dataframe[nrow(dataframe)+1,names(namedList)] <- namedList
    return(dataframe)
}

#' creates explicit metadata for action
#'
#' @param actionType should be one of DataImport, TLFGeneration, ReportGeneration, Edit, Move, Copy, AnalysisFileGeneration, Analysis or Other
#' @param actionDescription optional
#' @param actionName optional: should be passed only in special cases, is normally captured automatically
#' @param executedBy optional: should be passed only in special cases, is normally captured automatically
#' @param executed optional: should be passed only in special cases, is normally captured automatically
#' @param executionStarted optional
#' @param inputFilePaths vector of input filepaths; only one of inputFilePaths and inputFilesMetadataFrame may be given
#' @param outputFilePaths vector of output filepaths; only one of outputFilePaths and filesMetadataFrame may be given
#' @param filesMetadataFrame dataframe of output files metadata (see: createFilesMetadataFrame);only one of inputFilePaths/outputFilePaths and filesMetadataFrame may be given
#' @param offset
#'
#' @return
#' @export
#'
#' @examples
tStoreActionMetadata <-
  function( actionType = "Other",
            actionDescription = "",
            actionName = "",
            executedBy = "",
            executed = tlconst$OT,
            executionStarted = tlconst$OT,
            
            inputFilePaths,
            outputFilePaths,
            filesMetadataFrame,

            offset = 0) {
    # Integration test
    if(!captureMetadata()){return()}
   
    if ((hasArg(inputFilePaths) | hasArg(outputFilePaths)) & hasArg(filesMetadataFrame)) {
      errorMessage <- "Error in tStoreActionMetadata: only one of inputFilePaths/outputFilePaths and filesMetadataFrame may be given."
      logErrorMessage(errorMessage)
      stop(errorMessage)
    }

    tryCatch(
      withCallingHandlers({
  
        tStartAction(actionType = actionType, offset = offset + 1) # create action to keep source/target relationship
        #on.exit(tEndAction()) # does not allow overriding executed, because tEndAction is always executed at the very end
        
        actionInfo <- getActiveAction()
        if (actionDescription != "") {actionInfo$description <- actionDescription}
        if (actionName != "") {actionInfo$actionName <- actionName}
        if (executedBy != "") {actionInfo$executedBy <- executedBy}
        if (executionStarted != tlconst$OT) {actionInfo$executionStarted <- executionStarted}
        activityId <- ""
        
        if (hasArg(inputFilePaths)){
          for (filePath in inputFilePaths){
            tStoreFileMetadata(access = "read",filePath = filePath)
          }
        } 
        
        if (hasArg(outputFilePaths)){
          for (filePath in outputFilePaths){
            tStoreFileMetadata(access = "write",filePath = filePath)
          }
        }
        
        if (hasArg(filesMetadataFrame)){
          for (i in 1:nrow(filesMetadataFrame)){
            tStoreFileMetadata(  access = filesMetadataFrame[i,"access"],
                               filePath = filesMetadataFrame[i,"filePath"],
                               fileHash = filesMetadataFrame[i,"fileHash"],
                               repoType = filesMetadataFrame[i,"repoType"],
                               repoRoot = filesMetadataFrame[i,"repoRoot"],
                               repoPath = filesMetadataFrame[i,"repoPath"],
                            repoVersion = filesMetadataFrame[i,"repoVersion"],
                           repoModified = filesMetadataFrame[i,"repoModified"],
                         repoModifiedBy = filesMetadataFrame[i,"repoModifiedBy"],
                         qualityChecked = filesMetadataFrame[i,"qualityChecked"],
                            description = filesMetadataFrame[i,"description"] )
         }
        }
        
        tEndAction()
        if (executed != tlconst$OT) {actionInfo$executed <- executed}
      }, error = function(e) {
        writeToLog(e)
      }), error = function(e) {
        tEndAction()
      })
  }

#' Stores file metadata
#' @description this function can be called explicitly from users of traceLib if files are read or written by functions, where no t_wrapper is available
#'
#' @param access Should be one of read or Write
#' @param filePath local file path; if this is not given, the following parameters except for repoRoot, qualityChecked, description are mandatory
#' @param fileHash optional: only used, if filePath is not given
#' @param repoType optional:
#' @param repoRoot optional:
#' @param repoPath optional: 
#' @param repoVersion optional:
#' @param repoModified optional:
#' @param repoModifiedBy optional:
#' @param qualityChecked optional:
#' @param description optional:
#'
#' @return
#' @export
#'
#' @examples
#' tStoreFileMetadata(access = "read", filePath = "filepath")
tStoreFileMetadata <- function(access = c("read", "write"),
                               filePath = NA,
                               fileHash = NA,
                               repoType = NA,
                               repoRoot = NA,
                               repoPath = NA,
                               repoVersion = NA,
                               repoModified = NA,
                               repoModifiedBy = NA,
                               qualityChecked = NA,
                               description = NA) {
  # Integration test
  if(!captureMetadata()){return()}
  
  access <- match.arg(access) # check, that access is one of these values, TODO: error handling
  
  if (is.na(filePath)) { 
    if (is.na(fileHash) | is.na(repoType) | is.na(repoPath) | is.na(repoVersion) | is.na(repoModified) | is.na(repoModifiedBy) ) {
      errorMessage <- "Error in tStoreFileMetadata: without argument filePath all of the following arguments are required: fileHash, repoType, repoPath, repoVersion, repoModified, repoModifiedBy"
      logErrorMessage(errorMessage)
      stop(errorMessage)
    }
  }
  
  tryCatch(
    withCallingHandlers({
      if (!is.na(filePath)){
        fi <- FileInfo$new(init = "auto", access = access, filePath = filePath)
      } else {
        fi <- FileInfo$new(init = "manual", access = access)
        fi$fileHash <- fileHash
      }
        
      if (!is.na(repoType)) { fi$repoType <- repoType }
      if (!is.na(repoRoot)) { fi$repoRoot <- repoRoot }
      if (!is.na(repoPath)) { fi$repoPath <- repoPath }
      if (!is.na(repoVersion)) { fi$repoVersion <- repoVersion }
      if (!is.na(repoModified)) { fi$repoModified <- repoModified }
      if (!is.na(repoModifiedBy)) { fi$repoModifiedBy <- repoModifiedBy }
      if (!is.na(qualityChecked)) { fi$qualityChecked <- qualityChecked }
      if (!is.na(description)) { fi$description <- description }
      # ToDo: where are other file properties set,e.g. localCreatedBy, ..
      
      # Don't override a read/write access fileInfo by a  read access fileInfo to store the first  read, 
      # but   override a read/write access fileInfo by a write access fileInfo to store the latest write.
      if (access == "write" | !(fi$getKey() %in% names(tlvar$FILE_INFOS)) ) {
          tlvar$FILE_INFOS[[fi$getKey()]] <- fi
      }
      
      actionInfo <- getActiveAction()
      
      if (access == "read") {
        actionInfo$addInput(fi) # passing reference to the object instead of key or local file path
      }
      else if (access == "write") {
        fi$actionInfo <- paste(actionInfo$actionId, actionInfo$actionName, collapse = "-") # only fill, when file is written
        
        # only fill activityId, when file is written
        activityId <- determineActivityId(repoPath = fi$repoPath)
        if (activityId == "") { 
          activityId <- tlvar$ACTIVITY_ID 
        } else { 
          # if activityId was found and action or global activityId was empty, update it by this activityId,
          # because activity determination of output file path is probably ok and better than no activity
          if (actionInfo$activityId == "") { actionInfo$activityId <- activityId }
          if (tlvar$ACTIVITY_ID == "") { tlvar$ACTIVITY_ID <- activityId }
        }
        fi$activityId <- activityId 

        actionInfo$addOutput(fi) # only fill, when file is written
      }
      
    }, error = function(e) {
      writeToLog(e)
    }), error = function(e) {
      e
    })
}



