#' Wrapper function for read.csv to capture metadata
#'
#' @param file
#' @param ... pass on other arguments as you use the read.csv
#'
#' @return
#' @export
#'
#' @examples
#' tReadCsv("filepath")
tReadCsv <- function(file, ...) {
  # Integration test
  fileContent <- read.csv(file = file, ...)

  if(!captureMetadata()){return()}

  tStoreFileMetadata(access = "read", filePath = file)

  return(fileContent)
}

#' Wrapper function for read.csv to capture metadata
#'
#' @param file
#' @param ... pass on other arguments as you use the read.table
#'
#' @return
#' @export
#'
#' @examples
#' tReadTable("filepath")
tReadTable <- function(file, ...) {
  # Integration test
  fileContent <- read.table(file = file, ...)

  if(!captureMetadata()){return()}

  tStoreFileMetadata(access = "read", filePath = file)

  return(fileContent)
}


#' Wrapper function for write.csv to capture metadata
#'
#' @param x Data object
#' @param file filepath
#' @param ... other arguments to write.csv
#'
#' @return
#' @export
#'
#' @examples
#' tWriteCsv(x = data, file = "filepath")
tWriteCsv <- function(x, file, ...) {
  # Integration test
  result <- write.csv(x = x, file = file, ...)
  
  if(!captureMetadata()){return()}
  
  tStoreFileMetadata(access = "write", filePath = file)

  return(result)
}
