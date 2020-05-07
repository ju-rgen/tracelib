#' Title
#'
#' @param filePath
#'
#' @return
#' @export
#'
#' @examples
getFileType <- function(filePath) {
  # Unit test (Prio 2)
  extension <- tolower(file_ext(filePath))

  if (extension != "") {
    filetype <- tlconst$FILE_TYPE_LIST[[extension]]
  } else {
    filetype <- "Other"
  }
  if (is.null(filetype)) filetype <- "Other"

  return(filetype)
}

#' Title
#'
#' @param filePath
#'
#' @return
#' @export
#'
#' @examples
getFileHash <- function(filePath) {
  # Unit test
  checksum <- md5sum(path.expand(filePath))
  if (is.na(checksum)) {
    fileHash <- "NOHASH"
  } else {
    fileHash <- as.character(checksum)
  }
  
  return(fileHash)
}
