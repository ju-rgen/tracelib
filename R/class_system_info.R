#' A Reference Class to represent System information
#' @export
#' @field
SystemInfo <- setRefClass(
  "SystemInfo",
  fields = list(
    "systemId" = "numeric", # Primary key
    "systemName" = "character",
    "operatingSystem" = "character",
    "osVersion" = "character",
    "RVersion" = "character",
    "validated" = "logical",
    "description" = "character"
  ),

  methods = list(
    initialize = function() {
      systemId <<- 1
      info <- Sys.info()
      systemName <<- info[["nodename"]]
      operatingSystem <<- info[["sysname"]]
      osVersion <<- info[["release"]] # version
      # RVersion <<- R.version$version.string
      RVersion <<- paste(R.version$major, R.version$minor, sep = ".")
      validated <<- F
      description <<- ""
    },

    toList = function() {
      classFields <- getRefClass()$fields()
      objFieldValues <- list()
      for (fieldName in names(classFields)) {
        objFieldValues[[fieldName]] <- .self[[fieldName]]
      }
      return(objFieldValues)
    },

    toJSONObject = function() {
      # Unit test
      JSONObj <- toJSON(toList(),POSIXt = "ISO8601",
                        factor = "string", null = "null",
                        na = "string", pretty = T,)
      return(JSONObj)
    },

    fromList = function(obj) {
      classFields <- getRefClass()$fields()
      values <- obj
      values$systemId <- as.numeric(values$systemId)
      values$validated <- as.logical(values$validated)
      for (fieldName in names(classFields)) {
        .self[[fieldName]] <- values[[fieldName]]
      }
      return(.self)
    },

    addToFrame = function() {
      # Unit test
      df <- as.data.frame(toList())
      renameColumns(df, changelist = tlconst$SI_DB_MAP)
    },

    insertToDB = function(dbCon) {
      dbFields <- dbListFields(dbCon, c(tlconst$DB_SCHEMA, "system"))
      infosFrame <- addToFrame()
      dataToInsert <- infosFrame[, which((names(infosFrame) %in% dbFields) == TRUE)]

      dbWriteTable(dbCon, c(tlconst$DB_SCHEMA, "system"),
        value = dataToInsert, append = TRUE,
        row.names = FALSE
      )
    }
  )
)
