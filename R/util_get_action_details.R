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


