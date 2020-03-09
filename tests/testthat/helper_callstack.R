options(keep.source = TRUE)
#' Starts meta data capture for an action
#'
#' @return
#' @export
#'
#' @examples
#' testCallstack()
testCallstack <- function() {
  tryCatch(
    withCallingHandlers({
      # getCallingFunctionName(-4)
      #     tStartAction()
      getCallStack()
      getCallInfo()
    },
    error = function(e) {
      writeToLog(e)
    }
    ),
    error = function(e) {
      writeToLog(e)
    }
  )
  # Super action is always one level higher than action
}

call_action <- function() {
  tStartAction(actionType = "Other")
  # getCallInfo()
  # getCallingFunctionName(-1)
  tEndAction()
}

workflow <- function(storageMode = "File",outputFolder) {
  tStartMetadataCapture(metaDataCapture = T, actionType = "ImportFile")

  #tReadCsv(filePath)

  call_action()
  tEndMetadataCapture(storageMode = storageMode,
                      outputFolder = outputFolder,
                      jsonFileName = "test_workflow",
                      useTimeStamp = F)
}


initAi <- function(){
  ai <- ActionInfo$new(init="manual")
  ai$actionId <- 1
  ai$systemId <- 1
  ai$actionName = "Test action"
  ai$executedBy = "test author"
  ai$description = "Action info class for testing import and export"
  return(ai)
}


initConst()





