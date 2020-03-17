#' writeToLog
#'
#' @param error 
#'
#' @return
#' @export
#'
#' @examples
writeToLog <- function(error) {
  # Unit test ?!
  logEntry <- list(
    actionId = "",
    time = Sys.time(),
    errorMessage = error$message,
    errorCall = error$call,
    callStack = list()
  )

  tryCatch({
    currentAction <- getActiveAction()
    if (!is.null(currentAction)) {
      logEntry$actionId <- currentAction$actionId
    }

    logEntry$callStack <- getCallStackAsList(offset = 2) # remove rows for writeToLog and error handler
    # logCallStack(prefix = "error_") # temp
  }, error = function(e) {
    e
  })

  tlvar$ERROR_LOG <- append(tlvar$ERROR_LOG, logEntry)
}

#' logWarning
#'
#' @param error 
#'
#' @return
#' @export
#'
#' @examples
logWarning <- function(warning) {
  # Unit test ?!
  logEntry <- list(
    actionId = "",
    time = Sys.time(),
    warningMessage = warning$message,
    warningCall = warning$call,
    callStack = list()
  )
  
  tryCatch({
    currentAction <- getActiveAction()
    if (!is.null(currentAction)) {
      logEntry$actionId <- currentAction$actionId
    }
    
    logEntry$callStack <- getCallStackAsList(offset = 2) # remove rows for writeToLog and error handler
  }, error = function(e) {
    e
  })
  
  tlvar$ERROR_LOG <- append(tlvar$ERROR_LOG, logEntry)
}

logErrorMessage <- function(message) {
  # Unit test ?!
  print(message)
  logEntry <- list(
    actionId = "",
    time = Sys.time(),
    errorMessage = message,
    callStack = list()
  )
  
  tryCatch({
    currentAction <- getActiveAction()
    if (!is.null(currentAction)) {
      logEntry$actionId <- currentAction$actionId
    }
    
    logEntry$callStack <- getCallStackAsList(offset = 2) # remove rows for writeToLog and error handler
  }, error = function(e) {
    e
  })
  
  tlvar$ERROR_LOG <- append(tlvar$ERROR_LOG, logEntry)
}