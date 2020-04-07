#' A Reference Class to represent File information
#' @export
#' @field Filehash
FileInfo <- setRefClass("FileInfo",
  fields = list(
    "fileHash" = "character", # Primary key
    "repoType" = "character", # ToDo add to db property translation list, also for action description
    "repoPath" = "character",
    "repoVersion" = "character",
    "repoFileStatus" = "character",
    "repoModified" = "POSIXct",
    "repoModifiedBy" = "character",
    "localFilePath" = "character",
    "localModified" = "POSIXct",
    "localModifiedBy" = "character",
    "fileType" = "character",
    "groupId" = "character",
    "activityId" = "character",
    "qualityChecked" = "logical",
    "qualityComputed" = "logical",
    "description" = "character",

    "actionInfo" = "character", # just for temporary storage of action, which had written (or read) this file
    "accessInfo" = "character",
    "repoRoot" = "character"
  ),

  methods = list(
    initialize = function(init = "auto", access = c("read", "write"), filePath) {
      initializeEmpty(access)

      if (init == "auto") {
        fileHash <<- paste0("md5:", getFileHash(filePath))
        fileType <<- getFileType(filePath)

        localFilePath <<- filePath
        localModified <<- file.mtime(filePath)
        localModifiedBy <<- ""
        actionInfo <<- ""

        SVNinfo <- getSVNInfo(filePath)
        if (!is.null(SVNinfo)) {
          repoType <<- "SVN"
          repoVersion <<- SVNinfo[["revision"]] # strtoi(SVNinfo[["revision"]])
          if (repoVersion == "") repoVersion <<- "-1" # if (is.null(repoVersion)) repoVersion <<- "-1"
          repoRoot <<- SVNinfo[["repoRoot"]]
          repoPath <<- SVNinfo[["URL"]]
          repoModified <<- as.POSIXct(SVNinfo[["lastChanged"]])
          repoModifiedBy <<- SVNinfo[["author"]]
          repoFileStatus <<- getSVNStatus(filePath)
        }
        if (repoPath == "") {
          # repoPath <<- localFilePath
          # repoVersion <<- "-1"
          repoPath <<- paste(tlvar$SYSTEM_INFO[[1]]$systemName,localFilePath,sep='#')
          repoVersion <<- format(Sys.time(), "%y%m%d_%H%M%S")
          repoType <<- "Local"
        }
      }
    },

    initializeEmpty = function(access = c("read", "write")) {
      access <- match.arg(access)

      accessInfo <<- access # or .self ....

      fileHash <<- ""
      localFilePath <<- ""
      localModified <<- tlconst$OT
      localModifiedBy <<- ""
      actionInfo <<- ""

      repoType <<- ""
      repoModified <<- tlconst$OT
      repoVersion <<- "-1"
      repoRoot <<- ""
      repoPath <<- ""
      repoModifiedBy <<- ""
      repoFileStatus <<- ""

      groupId <<- ""
      activityId <<- ""
      qualityChecked <<- F
      qualityComputed <<- F
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
      # values <- fromJSON(obj)
      values <- obj
      values$repoModified <- as.POSIXct(values$repoModified, format="%Y-%m-%dT%H:%M:%S")
      values$localModified <- as.POSIXct(values$localModified, format="%Y-%m-%dT%H:%M:%S")
      for (fieldName in names(classFields)) {
        .self[[fieldName]] <- values[[fieldName]]
      }
      return(.self)
    },

    addToFrame = function() {
      # Unit test
      df <- as.data.frame(toList(), stringsAsFactors = FALSE)
      df[["activityId"]] <- setNAifEmpty(.self$activityId)
      df[["groupId"]] <- setNAifEmpty(.self$groupId)
      renameColumns(df, changelist = tlconst$FI_DB_MAP)
    },

    getKey = function() {
      if (.self$localFilePath != "") {
        key <- paste0(.self$fileHash, ";", .self$localFilePath)
      } else {
        key <- paste0(.self$fileHash, ";", .self$repoPath)
      }
      return(key)
    }
    # ,
    #  JJ 2020-04-03 separate DB code
    # insertToDB = function(dbCon) {
    #   dbFields <- dbListFields(dbCon, c(tlconst$DB_SCHEMA, "file"))
    #   infosFrame <- addToFrame()
    #   dataToInsert <- infosFrame[, which((names(infosFrame) %in% dbFields) == TRUE)]
    # 
    #   if (fileInfoPrimaryKeyExists(dbCon, .self$fileHash, .self$repoPath, .self$repoVersion) == FALSE) {
    #     # print(paste(.self$fileHash,.self$repoPath,.self$repoVersion,collapse = "-"))
    #     dbWriteTable(dbCon, c(tlconst$DB_SCHEMA, "file"),
    #       value = dataToInsert, append = TRUE,
    #       row.names = FALSE
    #     )
    #   } else {
    #     if (.self$accessInfo == "write") {
    #       warning(paste0( # Change this to error/write warnings to log file
    #         "Could not overwrite file meta data for ", .self$localFilePath,
    #         " for action ", .self$actionInfo, " as the file is unchanged"
    #       ))
    #     }
    #   }
    # }
  )
)
