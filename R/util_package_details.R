#' Title
#'
#' @param func
#'
#' @return
#' @export
#'
#' @examples
getFunctionEnv <- function(funcName) {
  # funct <- getFunction(func,mustFind = T)
  # env <- environmentName(environment(funct))
  env <- environmentName(findFunction(funcName)[[1]])
  return(env)
}

#' Title
#'
#' @param func
#'
#' @return
#' @export
#'
#' @examples
getPackageDetails <- function(envName) {
  # Unit test ? (with some default package)
  tryCatch({
    pkgDesc <- packageDescription(envName)
    pkgAuthor <-str_trim(str_trunc(str_replace(pkgDesc$Author,
                                               "\\[(.)*\\]",
                                               replacement = ""),
                                   width = 32,side = "right"))
    pkgDetails <- list(
      package = envName,
      version = pkgDesc$Version,
      author = pkgAuthor,
      details = paste(envName, pkgDesc$Version, pkgDesc$Built, " | "),
      built = "1970-01-01 00:00:00 UTC",
      warnings = ""
    )
  },
  warning = function(e) {
    pkgDetails <- list(
      package = paste0(envName, "isNoPackage"),
      version = "NoPackage",
      author = "NoPackage",
      details = "NoPackage",
      built = "1970-01-01 00:00:00 UTC",
      warnings = ""
    )
    e
  }
  )

  tryCatch({
    if (is.character(pkgDesc$Built)) {
      pkgDetails$built <- unlist(strsplit(pkgDesc$Built, "; "))[3]
    } else {
      pkgDetails$warnings <- "No Built found"
    }
  }, error = function(e) {
    e
  })

  return(pkgDetails)
}
