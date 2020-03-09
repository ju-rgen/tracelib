#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
pastePath <- function(path = "clipboard") {
  y <- if (path == "clipboard") {
    readClipboard()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  x <- str_remove_all(x, "\"")
  writeClipboard(x)
  return(x)
}

#' Title
#'
#' @param df
#' @param changelist
#'
#' @return
#' @export
#'
#' @examples
renameColumns <- function(df, changelist) {
  for (i in 1:length(names(df))) {
    if (length(changelist[[names(df)[i]]]) > 0) {
      names(df)[i] <- changelist[[names(df)[i]]]
    }
  }
  return(df)
}

#' Title
#'
#' @param configFile
#'
#' @return
#' @export
#'
#' @examples
prepareFileTypeConfigGlist <- function(configFile) {
  # Unit test (Prio 2)
  FileTypeConfig <- readLines(configFile)
  # FileTypeConfig <- configFile
  FileTypeConfigList <- list()

  for (i in 1:length(FileTypeConfig)) { # nrow
    type <- unlist(strsplit(as.character(FileTypeConfig[i]), ",")) # ,1
    if (length(type) > 1) {
      for (j in 2:length(type)) {
        FileTypeConfigList[type[[j]]] <- type[[1]]
      }
    }
  }
  return(FileTypeConfigList)
}


# Function to return Sentence case word
#' Title
#'
#' @param string
#'
#' @return
#' @export
#'
#' @examples
capStr <- function(string) {
  c <- strsplit(string, " ")[[1]]
  paste(toupper(substring(c, 1, 1)), substring(c, 2),
    sep = "", collapse = " "
  )
}


#' Title
#'
#' @param field
#'
#' @return
#' @export
#'
#' @examples
setNAifEmpty <- function(field) {
  if (missing(field) | field == "") {
    return(NA)
  } else {
    return(field)
  }
}

#' Checks if Metadata capture value is set
#'
#' @return
#' @export
#'
#' @examples
captureMetadata <- function(){
  if(!is.environment(tlvar)){
    return(FALSE)
  }
  if(!exists("METADATA_CAPTURE",envir = tlvar)){
    return(FALSE)
  }
  return(tlvar$METADATA_CAPTURE) 
}
