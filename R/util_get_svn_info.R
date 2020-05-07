#' Title
#'
#' @param filePath
#'
#' @return
#' @export
#'
#' @examples
getFullFilePath <- function(filePath) {
  fp <- file.path(filePath)
  fp <- str_replace(fp, "~", path.expand("~"))
  return(fp)
}

#' Title
#'
#' @param filePath
#'
#' @return
#' @export
#'
#' @examples
getSVNInfo <- function(filePath) {
  # Integration test
  
  #ToDo: check, if creation of environments tlvar, tlconst is possible in function tStartMetadataCapture instead of in sourcing of file initGlobals.R
  if (exists("tlconst")) {
    if (exists("READ_SVN_INFO",tlconst)) {
      if (tlconst$READ_SVN_INFO == FALSE) { return(NULL) }
   }
  }
  
  cmd <- sprintf('svn info "%s"', getFullFilePath(filePath))

  tryCatch({
    x <- system(cmd, intern = TRUE, )
    revision <- extractTagValue(x, "Revision")
    repoRoot <- extractTagValue(x, "Repository Root")
    lastChanged <- extractTagValue(x, "Last Changed Date")
    URL <- extractTagValue(x, "URL")
    author <- extractTagValue(x, "Last Changed Author")

    result <- list(
      revision = revision,
      repoRoot = repoRoot,
      lastChanged = lastChanged,
      URL = URL,
      author = author
    )
    return(result)
  },
  warning = function(cond) {
    message(paste0("Warning in getSVNInfo: file ", filePath, " not in SVN"))
    return(NULL)
  },
  error = function(cond) {
    message(paste("Error in getSVNInfo", cond, " not in SVN"))
    return(NULL)
  }
  )
}


#' Title
#'
#' @param svninfo
#' @param tag
#'
#' @return
#' @export
#'
#' @examples
extractTagValue <- function(svninfo, tag) {
  tagX <- paste0(tag, ":")
  tagY <- paste0(tagX, " ")
  value <- unlist(strsplit(grep(tagX, svninfo, value = T), tagY))[2]
  return(value)
}


#' Title
#'
#' @param aPath
#'
#' @return
#' @export
#'
#' @examples
getSVNStatus <- function(filePath) {
  # Integration test
  if (exists("tlconst")) {
    if (tlconst$READ_SVN_INFO == FALSE) { return("-N") }
  }
  cmd <- sprintf('svn status "%s"', filePath)

  svnstatus <- tryCatch({
    system(cmd, intern = TRUE)
  },
  warning = function(cond) {
    message("SVN status not found") # TODO: + message
    return("-W")
  },
  error = function(cond) {
    message("SVN status not found") # TODO: + message
    return("-E")
  }
  )
  statusString <- strtrim(svnstatus, 7)
  if (length(statusString) > 0) {
    return(statusString)
  } else {
    return("")
  }
}
# svn status results, see https://stackoverflow.com/questions/2034/what-do-the-result-codes-in-svn-mean/6028405
# ?: This file is not under version control
# M: Working copy is modified
# U: Working file was updated
# G: Changes on the repo were automatically merged into the working copy
# C: This file conflicts with the version in the repo
# !: This file is under version control but is missing or incomplete
# A: This file will be added to version control (after commit)
# A+: This file will be moved (after commit)
# D: This file will be deleted (after commit)
# S: This signifies that the file or directory has been switched from the path of the rest of the working copy (using svn switch) to a branch
# I: Ignored
# X: External definition
# ~: Type changed
# R: Item has been replaced in your working copy. This means the file was scheduled for deletion, and then a new file with the same name was scheduled for addition in its place.
# L : Item is locked
# E: Item existed, as it would have been created, by an svn update.

# -E: Error in system call, see getSVNStatus.
# -W: Warning in system call, see getSVNStatus.
