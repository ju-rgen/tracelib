#' test DB connection
#'
#' @return >= 0, if dbConnection works fine, -1 else
#' @export
#'
#' @examples
getDBCon <- function(credFile) {
  # Unit test
  # Check DB if connection exists
  dbCon <- tryCatch({
    credPath <- system.file(credFile, package = "tracelib", mustWork = F)
    if (credPath == ''){
      logErrorMessage(paste0('No DB connection established, because file ',credFile,' does not exist in package tracelib'))
      return(NULL)
    }
    cred <- fromJSON(credPath)

    dbConnect(
      drv = cred$drv,
      host = cred$host,
      user = cred$user,
      password = cred$password,
      dbname = cred$dbname
    )
  }, warning = function(w) {
    writeToLog(w)
    return(NULL)
  }, error = function(e) {
    writeToLog(e)
    return(NULL)
  })
}

#' test DB connection
#'
#' @return >= 0, if dbConnection works fine, -1 else
#' @export
#'
#' @examples
testDBCon <- function(){

  dbCon <- getDBCon("keys.json")
  if (is.null(dbCon)) {
    return(-1)
  }
  on.exit(dbDisconnect(dbCon))
  
  tryCatch({
    id <- getLastSystemId(dbCon)
    return(id)
  }, error = function(e) {
    print("Error in access to DB")
    writeToLog(e)
    return(-1)
  })
  
}

#' get single value from database
#'
#' @param dbCon
#' @param query
#'
#' @return
#' @export
#'
#' @examples
querySingleValue <- function(dbCon, query) {
  dfResult <- dbGetQuery(dbCon, query)
  if (is.null(dfResult)) {
    warning("result is empty")
    return(NULL)
  }
  else if (nrow(dfResult) != 1 | ncol(dfResult) != 1) {
    warning("does not return a single value")
    return(NULL)
  }
  else {
    return(dfResult[[1]])
  }
}

fileInfoPrimaryKeyExists <- function(dbCon, fileHash, repoPath, repoVersion) {
  query <- paste0(
    "Select * from ",tlconst$DB_SCHEMA,".file where file_hash = '", fileHash,
    "' and repo_path = '", repoPath,
    "' and repo_version = '", repoVersion, "'"
  )
  res <- dbGetQuery(dbCon, query)
  if (nrow(res) > 0) {
    TRUE
  } else {
    FALSE
  }
}

getLastSystemId <- function(dbCon) {
  return(querySingleValue(dbCon, paste0("SELECT MAX(system_id) FROM ",tlconst$DB_SCHEMA,".system")))
}


getLastActionId <- function(dbCon) {
  return(querySingleValue(dbCon, paste0("SELECT MAX(action_id) FROM ",tlconst$DB_SCHEMA,".action")))
}


getActivityIdForRepoPathExact <- function(dbCon, repoPath) {
  query <- paste0("SELECT activity_id FROM ",tlconst$DB_SCHEMA,".activity WHERE repo_path = '", repoPath, "'")
  return(querySingleValue(dbCon, query))
}

getActivityIdForRepoPath <- function(dbCon, repoPath) {
  # Unit test (or automated integration test - together with other DB functions?)
  query <- paste0(
    "select activity_id,repo_path,length(repo_path) from ",tlconst$DB_SCHEMA,".activity ",
    "where '", repoPath, "' LIKE repo_path || '%' order by length(repo_path) DESC"
  )
  dfResult <- dbGetQuery(dbCon, query)
  if (is.null(dfResult)) {
    warning(paste0("No activity_id found (null) for repoPath = ", repoPath))
    return("")
  } else if (nrow(dfResult) == 0) {
    warning(paste0("No activity_id found for repoPath = ", repoPath))
    return("")
  } else if (nrow(dfResult) == 1) {
    return(dfResult$activity_id[[1]])
  } else if (dfResult$length[[1]] == dfResult$length[[2]]) {
    warning(paste0("No unique activity found: ", dfResult$activity_id[[1]], " and ", dfResult$activity_id[[2]], " for repoPath = ", repoPath))
    return("")
  } else {
    return(dfResult$activity_id[[1]])
  }
}
