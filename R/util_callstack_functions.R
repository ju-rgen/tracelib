# options(keep.source = TRUE)
#' get callStack as dataframe
#' @description Returns information about the current callstack adjusted for "implicit" function calls like tryCatch.
#' getCallstack can be used to return information in case of errors or to determine the calling function when creating an actionInfo
#' @param offset - to cut the last rows of the callstack
#' @param excludeEnvironments ("implicit" functions from these environments are not included into the callstack)
#'
#' @return dataframe with rows per call, top level call in row 1; and columns envName, funcName,
#' scriptName: contains name of scriptfile, where file of script is given for called function, contains name of package of function otherwise;
#' scriptPath: contains path of scriptfile or package details respectively (see scriptName)
#' @export
#'
#' @examples
getCallStack <- function(offset = 0, excludeEnvironments = c("base", "methods")) {
  # Unit test
  df <- data.frame(
    envName = character(),
    funcName = character(),
    scriptName = character(),
    scriptPath = character(),
    stringsAsFactors = FALSE
  )

  errorOccured <- 0

  for (i in 1:sys.nframe()) {
    call <- sys.call(i)
    func <- sys.function(i)

    tryCatch({
      funcSymbol <- as.list(call)[[1]] # normally typeof(funcSymbol) = symbol, class(funcSymbol) = name
      # there are cases, where typeof(funcSymbol) = "closure" or "language"
      # (e.g. when assigning a string to a boolean variable/field)
      # then toString(funcSymbol) throws exception itself

      if (typeof(funcSymbol) != "symbol" & typeof(funcSymbol) != "language") {
        next
      } # or  =="closure"    != "symbol"   Problem actionInfo$addInput not symbol ....

      funcName <- toString(funcSymbol)
      betterFuncName <- ""
      
      if (typeof(funcSymbol) == "symbol") {
        # in case of indirect call via do.call choose 2nd argument
        if (funcName == "do.call") {
          argumentFuncName <- toString(as.list(call)[[2]])
          betterFuncName <- paste0(funcName, "=", argumentFuncName) # add the name of the passed function
          func <- match.fun(argumentFuncName)
        }

        # in case of funcSymbol is a function passed as a parameter the funcSymbol has just the parameter name, not the function name passed
        else if (i > 1) {
          if (funcName %in% names(sys.call(i - 1))) {
            argumentFuncName <- sys.call(i - 1)[[funcName]]
            betterFuncName <- paste0(funcName, "=", argumentFuncName) # add the name of the passed function
            func <- match.fun(argumentFuncName)
          }
        }

      # in case of object method call
      } else if (typeof(funcSymbol) == "language" & length(funcSymbol) == 3) {
        operatorName <- toString(funcSymbol[[1]])
        methName <- toString(funcSymbol[[3]])
        
        if (typeof(funcSymbol[[2]]) == "symbol"){
           varName <- toString(funcSymbol[[2]]  )
        
          # ensure that it is an object method and funcSymbol[[2]] is a variable name (e.g. not true for initialize call)
          # variable is not defined in this environment, but in parent environment
          if (operatorName == "$" & exists(varName, envir = sys.frames()[[i-1]], inherits = TRUE)){
            # add class name
            varObj <- get0(varName, envir = sys.frames()[[i-1]], inherits = TRUE)
            class_name <- as.character(class(varObj)[[1]]) # class(varObj) can return a vector, e.g. "R6C" "R6"
            betterFuncName <- paste0( class_name, "$", methName )
          }
          
        } else if (typeof(funcSymbol[[2]]) == "language" & length(funcSymbol[[2]]) == 3){
          varTriple <- funcSymbol[[2]]
          
          operator2Name <- toString(varTriple[[1]])
          var2Name <- toString(varTriple[[2]])
          field2Name <- toString(varTriple[[3]])
          
          # ensure that it is an object method and funcSymbol[[2]] is a variable name (e.g. not true for initialize call)
          # variable is not defined in this environment, but in parent environment
          if (operator2Name == "$" & existsInEnvIdx(var2Name, i-1)){ # e.g. self$simulatePopulation$runTask
            # add class name
            var2Obj <- getFromEnvIdx(var2Name, i-1)
            varObj <- var2Obj[[field2Name]]
            class_name <- as.character(class(varObj)[[1]]) 
            betterFuncName <- paste0( class_name, "$", methName )
          } else if (operator2Name == "[[" & existsInEnvIdx(var2Name, i-1) & existsInEnvIdx(field2Name, i-1)){ # e.g. self[[plotTask]]$runTask
            # add class name
            field2Obj <- getFromEnvIdx(field2Name, i-1)
            var2Obj <- getFromEnvIdx(var2Name, i-1)
            varObj <- var2Obj[[field2Obj]]
            class_name <- as.character(class(varObj)[[1]]) 
            betterFuncName <- paste0( class_name, "$", methName )
          } 
          
        } 

      } 
      if (betterFuncName != ""){
        funcName <- betterFuncName
      }

      envName <- getEnvironmentName(environment(func))
      scriptName <- getSrcFilename(func)

      if (envName %in% excludeEnvironments) {
        next # to exclude tryCatch, tryCatchOne, tryCatchList etc.
      } else if (length(scriptName) > 0) {
        scriptPath <- normalizePath(getSrcDirectory(func), winslash = "/")
        scriptPath <- paste0(scriptPath, "/", scriptName)
      } else {
        pkgDetails <- getPackageDetails(envName)
        scriptName <- pkgDetails$package
        scriptPath <- pkgDetails$details
      }
      df[nrow(df) + 1, ] <- list(envName = envName, funcName = funcName, scriptName = scriptName, scriptPath = scriptPath)
    },
    error = function(e) {
      errorOccured <- 1
    }
    ) # df[nrow(df) + 1, ] <- list("Error",e$message,"","")

    if (errorOccured == 1) {
      break
    }
  }

  nskip <- offset + 1 # offset + 1 : remove row for call of this function itself
  if (nskip >= nrow(df)) {
    nskip <- nrow(df) - 1
  } # in case of wrong offset return the first row of df
  return(head(df, -nskip))
}

existsInEnvIdx <- function(varName, envIdx) {
  return( exists(varName, envir = sys.frames()[[envIdx]], inherits = TRUE) )
}

getFromEnvIdx <- function(varName, envIdx) {
  return( get0(varName, envir = sys.frames()[[envIdx]], inherits = TRUE) )
}

#' getEnvironmentName
#'
#' @param env enviroment
#'
#' @return name of environment
#' @export
#'
#' @examples
getEnvironmentName <- function(env) {
  # Unit test
  # get name of environment, in case of unnamed environments return parent or ancestor environment name, e.g. in case of some class methods
  # see https://rstudio-pubs-static.s3.amazonaws.com/278710_bb8897865caf43c6a39757278547b1f4.html and https://adv-r.hadley.nz/environments.html
  envName <- environmentName(env)
  while (envName == "" & length(env) > 0) {
    env <- parent.env(env)
    envName <- environmentName(env)
  }
  return(envName)
}


#' getCallStackAsList
#'
#' @param offset - to cut the last rows of the callstack
#' @param excludeEnvironments ("implicit" functions from these environments are not included into the callstack)
#'
#' @return
#' @export
#'
#' @examples
getCallStackAsList <- function(offset = 0, excludeEnvironments = c("base", "methods")) {
  callStack <- getCallStack(offset = offset + 1, excludeEnvironments = excludeEnvironments) # offset + 1 : remove row for call of this function itself
  
# TODO: cover case, that tlvar does not exist, see in tStoreFileMetadata
  if(!exists("ERROR_LOG_MODE",envir = tlvar)){
    tlvar$ERROR_LOG_MODE == "Standard"
  }
  if (tlvar$ERROR_LOG_MODE == "Extended") {
    callStackList <- paste(callStack$envName, callStack$funcName, callStack$scriptName, callStack$scriptPath, sep = ":")
  } else {
    callStackList <- paste(callStack$envName, callStack$funcName, sep = ":")
  }
  return(callStackList)
}

#' getCallStackAsString
#'
#' @param offset - to cut the last rows of the callstack
#'
#' @return
#' @export
#'
#' @examples
getCallStackAsString <- function(offset = 0) {
  callStackList <- getCallStackAsList(offset = offset + 1) # offset + 1 : remove row for call of this function itself
  callStackString <- paste(callStackList, collapse = "/")
  return(callStackString)
}

#' getCallInfo
#' returns information about a calling function 
#' @param offset 0 for function directly calling getCallInfo, n for function n levels above in call hierarchy
#'
#' @return list with elements for envName, funcName,
#' scriptName: contains name of scriptfile, where file of script is given for called function, contains name of package of function otherwise;
#' scriptPath: contains path of scriptfile or package details respectively (see scriptName)
#' @export
#'
#' @examples
getCallInfo <- function(offset = 0) {
  # Unit test
  dfCallStack <- getCallStack(offset = offset + 1) # offset + 1 : remove row for call of this function itself
  n <- nrow(dfCallStack)
  callInfo <- as.list(dfCallStack[n, ])
  return(callInfo)
}



#' logCallStack
#' @description For test purposes only: writes complete callstack to a file in CALLSTACKLOGDIR, if and only if this global variable is given
#' for test reasons
#' @param prefix file name consists of prefix and timestamp
#' @param excludeEnvironments ("implicit" functions from these environments are not included into the callstack)
#'
#' @return
#'
#' @examples
logCallStack <- function(prefix = "cs_", excludeEnvironments = c("base", "methods")) {
  tryCatch({
    dfCallstack <- getCallStack(offset = 2, excludeEnvironments = excludeEnvironments)
    if (exists("CALLSTACKLOGDIR")) {
      t <- as.POSIXlt(Sys.time(), "GMT")
      filename <- paste0(prefix, toString(t$hour), toString(t$min), toString(floor(t$sec * 1000)), ".csv")
      write.csv(dfCallstack, paste0(CALLSTACKLOGDIR, "/", filename))
    }
  }, error = function(e) {
    e
  })
}


#' setGlobalStackInfos
#' @description For test purposes only: provides information about R calls, frames, functions where this function is called to the global variable STACKINFOS
#'
#' @return
#'
#' @examples
setGlobalStackInfos <- function() {
  functions <- list()
  for (i in 1:sys.nframe()) {
    functions <- append(functions, sys.function(i))
  }
  STACKINFOS <<- list("calls" = sys.calls(), "frames" = sys.frames(), "functions" = functions)
}
