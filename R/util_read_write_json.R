#' Title
#'
#' @param fileInfos
#' @param actionInfos
#' @param outputFolder
#'
#' @return
#' @export
#'
#' @examples
writeInfosToJson <- function(fileInfos, actionInfos, systemInfo, outputFolder, jsonFileName, useTimeStamp) {
  actInfoList <- lapply(actionInfos, function(x) x$toList())
  filInfoList <- lapply(fileInfos, function(x) x$toList())
  sysInfo <- lapply(systemInfo, function(x) x$toList())

  combinedList <- list(
    actionInfos = actInfoList,
    fileInfos = filInfoList,
    systemInfo = sysInfo
  )
  jsonObj <- toJSON(combinedList,
    POSIXt = "ISO8601",
    factor = "string", null = "null",
    na = "string", pretty = T,
  )

  writeJsonObjectToFile(
    jsonObj = jsonObj,
    outputFolder = outputFolder,
    jsonFileName = jsonFileName,
    useTimeStamp = useTimeStamp
  )
}

#' Title
#'
#' @param outputFolder
#' @param errorLog
#'
#' @return
#' @export
#'
#' @examples
writeJsonObjectToFile <- function(jsonObj, outputFolder, jsonFileName = "", useTopLevelAction = F, useTimeStamp = T) {
  # Unit test
  fileName <- ifelse(jsonFileName == "", "metadata", jsonFileName)

  if (useTopLevelAction & length(tlvar$ACTION_INFOS) > 0) {
    fileName <- paste(fileName, tlvar$ACTION_INFOS[[1]]$actionName, sep = "_")
  }

  if (useTimeStamp) {
    fileName <- paste(fileName, format(Sys.time(), "%Y_%m_%d_%H_%M_%OS3"), sep = "_")
  }

  fileName <- paste0(fileName, ".json")

  if (outputFolder != "") {
    write(jsonObj, paste0(outputFolder, fileName))
  }
}

#' Title
#'
#' @param outputFolder
#' @param errorLog
#'
#' @return
#' @export
#'
#' @examples
writeErrorLogToJson <- function(errorLog, outputFolder) {
  jsonObj <- toJSON(errorLog,
    POSIXt = "ISO8601",
    factor = "string", null = "null",
    na = "string", pretty = T,
  )

  writeJsonObjectToFile(
    jsonObj = jsonObj,
    outputFolder = outputFolder,
    jsonFileName = "tracelib_error_log",
    useTimeStamp = T
  )

}

#' Title
#'
#' @param jsonFile
#'
#' @return
#' @export
#'
#' @examples
readInfosFromJson <- function(jsonFile) {
  # Unit test
  infos <- fromJSON(jsonFile,simplifyDataFrame = F)

  filInfoList <- infos$fileInfos
  actInfoList <- infos$actionInfos
  sysInfoList <- infos$systemInfo 

  fileInfos <- list()
  actionInfos <- list()
  systemInfos <- list()
  
  for(i in 1:length(sysInfoList)){
    si <- SystemInfo$new()
    si <- si$fromList(sysInfoList[[i]]) #Currently system id is the key, is there a better way to do this?
    systemInfos[[as.character(si$systemId)]] <- si
  }


  for (i in 1:length(filInfoList)) {
    fi <- FileInfo$new(init = "manual", access = "read")
    fi <- fi$fromList(filInfoList[[i]])
    fileInfos[[fi$getKey()]] <- fi
  }

  for (i in 1:length(actInfoList)) {
    ai <- ActionInfo$new(init = "manual")
    actInfoList[[i]]$scriptFileInfo <- fileInfos[[actInfoList[[i]]$scriptFileInfo]]
    actInfoList[[i]]$inputFileInfos <- getFileInfos(actInfoList[[i]]$inputFileInfos, fileInfos)
    actInfoList[[i]]$outputFileInfos <- getFileInfos(actInfoList[[i]]$outputFileInfos, fileInfos)

    ai <- ai$fromList(actInfoList[[i]])
    actionInfos[[as.character(ai$actionId)]] <- ai
  }

  return(list(
    fileInfos = fileInfos,
    actionInfos = actionInfos,
    systemInfos = systemInfos
  ))
}


#' Title
#'
#' @param keyList
#' @param fileInfos
#'
#' @return
#' @export
#'
#' @examples
getFileInfos <- function(keyList, fileInfos) {
  fi <- lapply(keyList, function(x) if (!is.null(x)) {
      fileInfos[[x]]
    } else {
      list()
    })
  return(fi)
}
