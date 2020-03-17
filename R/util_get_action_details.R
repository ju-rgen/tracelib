options(keep.source = TRUE)

#' Title
#'
#' @return
#' @export
#'
#' @examples
getCallingFunctionName <- function(frame = -4) {
  funcName <- as.list(sys.call(frame))[[1]]

  func <- sys.function(frame)

  scriptName <- getSrcFilename(func)

  scriptPath <- normalizePath(getSrcDirectory(func), winslash = "/")

  if (length(scriptName) > 0) {
    scriptPath <- paste0(scriptPath, "/", scriptName)
  } else {
    scriptPath <- ""
  }

  callingFuncList <- list(
    "funcName" = funcName,
    "scriptName" = scriptName,
    "scriptPath" = scriptPath
  )
  return(callingFuncList)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
getActiveAction <- function() {
  # Unit test (Prio 2)
  if (!exists("ACTION_STACK", envir = tlvar)) {
    return(NULL)
  }
  lenStack <- length(tlvar$ACTION_STACK)
  if (lenStack == 0) {
    return(NULL)
  }
  ai <- tlvar$ACTION_STACK[[lenStack]]
  return(ai)
}

#' determineActivityId
#'
#' @param repoPath optional: path of folder or file in repository
#' @param filePath optional: local path of folder or file in checkout folder
#'
#' @return
#' @export
#'
#' @examples
determineActivityId <- function(repoPath = "", filePath = "") {

  dbCon <- getDBCon("keys.json")
  if (is.null(dbCon)) {
    logErrorMessage("No activityId determined due to missing DB Connection.")
    return("")
  }
  on.exit(dbDisconnect(dbCon))
  
  if (repoPath == "" | is.na(repoPath)) {
    
    if (filePath != "" & !is.na(filePath)) {
      SVNinfo <- getSVNInfo(filePath)
      if (!is.null(SVNinfo)) {
        repoPath <- SVNinfo[["URL"]]
      }
    } else {
      scriptFileInfo <- getActiveAction()$scriptFileInfo 
      if (!is.null(scriptFileInfo)) {
        repoPath <- scriptFileInfo$repoPath
      }
    }
  }
  
  if (is.null(repoPath)) {
    return("")
  } 
  if (str_length(repoPath) < 5) {
    return("")
  } # Foreign key constraint need to change this
  
  activityId <- getActivityIdForRepoPath(dbCon = dbCon, repoPath = repoPath)
  return(activityId)
}

