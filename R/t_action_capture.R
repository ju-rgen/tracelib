#' Starts meta data capture for an action
#'
#' @param actionType Should be one of DataImport, TLFGeneration, ReportGeneration, Edit, Move, Copy, AnalysisFileGeneration, Analysis or Other
#' @param offset optional: if action should be defined by a calling function of higher level
#'
#' @return
#' @export
#'
#' @examples
#' tStartAction(actionType = "DataImport")
tStartAction <- function(actionType = "Other", actionName = NA, description = NA, offset = 0) {
  # Integration test
  if(!captureMetadata()){return()}
  
  tryCatch(withCallingHandlers({
    ai <- ActionInfo(offset = offset + 1, actionType = actionType, actionName = actionName, description = description) # offset + 1 : remove row for call of this function itself

    # set SuperAction if available; Super action is always one level higher than action
    asLen <- length(tlvar$ACTION_STACK)
    if (asLen > 0) {
      ai$superAction <- tlvar$ACTION_STACK[[asLen]]
      ai$superActionId <- ai$superAction$actionId
    } else {
      ai$superActionId <- NaN
    }

    ai$executionStarted <- Sys.time()

    actionId <- length(tlvar$ACTION_INFOS) + 1
    ai$actionId <- actionId
    # ai$activityId <- tlvar$ACTIVITY_ID #  JJ 2020-04-03 separate DB code

    fi <- ai$scriptFileInfo # [[1]]
    tlvar$FILE_INFOS[[fi$getKey()]] <- fi


    tlvar$ACTION_INFOS[[as.character(actionId)]] <- ai

    tlvar$ACTION_STACK <- append(tlvar$ACTION_STACK, ai)
  }, error = function(e) {
    writeToLog(e)
  }), error = function(e) {
    e
  })
}


#' Indicates end of action
#'
#' @return
#' @export
#'
#' @examples
tEndAction <- function(offset = 0) {
  # Integration test
  if(!captureMetadata()){return()}
  
  tryCatch(
    withCallingHandlers({
      
      ai <- getActiveAction()
      
      funcInfo <- getCallInfo(offset = offset + 1) 
      functionName <<- as.character(funcInfo$funcName)
      if (ai$actionName != functionName) {
        errorMessage <- paste0("Error in tEndAction: activeActionName = ",ai$actionName," != ",functionName," = current functionName")
        logErrorMessage(errorMessage)
       }

      ai$executed <- Sys.time()
      
      # Question: is there a way in R to call tEndAction, when leaving  a function without writing it before every return statement in that function?
      
      # Remove the last element of actionStack list
      lenStack <- length(tlvar$ACTION_STACK)
      if (lenStack > 0) {
        tlvar$ACTION_STACK[[lenStack]] <- NULL
      }
      # ToDo? check if ActionStack is empty, if empty there is an error.
      
    }, error = function(e) {
      writeToLog(e)
    }), error = function(e) {
      e
    })
}

