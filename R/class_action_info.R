#' A Reference Class to represent Action information
#' @export
#' @field
ActionInfo <- setRefClass(
  "ActionInfo",
  fields = list(
    "actionId" = "numeric", # Primary key
    "actionName" = "character",
    "actionType" = "character",
    "superAction" = "ANY",
    "superActionId" = "numeric", # should just be used during persisting the object to file or DB
    "scriptFileInfo" = "ANY",
    "inputFileInfos" = "list",
    "outputFileInfos" = "list",
    "executionStarted" = "POSIXct",
    "executed" = "POSIXct",
    "executedBy" = "character",
    "activityId" = "character",
    "systemId" = "numeric",
    "groupId" = "character",
    "description" = "character"
  ),

  methods = list(
    initialize = function(init = "auto", offset = 0, actionType = "Other") {
      # Integration test

      initializeEmpty()

      if (init == "auto") {
        funcInfo <- getCallInfo(offset = offset + 2) # offset + 2 : remove row for call of class and this method
        actionName <<- as.character(funcInfo$funcName)
        actionType <<- actionType

        # If the function is part of an installed package, there is no file path available (mostly)
        if (funcInfo$envName == "R_GlobalEnv") {
          addScriptFileInfo(funcInfo$scriptPath)
        } else {
          addScriptPackageInfo(funcInfo$envName)
        }

        executionStarted <<- Sys.time()
        executed <<- Sys.time()
        executedBy <<- Sys.info()[["user"]]
        systemId <<- 1
        groupId <<- "1" # Always the same

        description <<- getCallStackAsString(offset = offset + 1) #TODO: should we keep this?
      }
    },

    initializeEmpty = function() {
      actionId <<- numeric(0)
      actionName <<- ""
      actionType <<- "Other"
      superActionId <<- NaN # foreign key constraint
      superAction <<- NULL
      scriptFileInfo <<- NULL
      inputFileInfos <<- list()
      outputFileInfos <<- list()
      executionStarted <<- tlconst$OT 
      executed <<- tlconst$OT
      executedBy <<- ""
      activityId <<- ""
      systemId <<- numeric(0)
      groupId <<- ""
      description <<- ""
    },

    addInput = function(fi) {
      inputFileInfos[[fi$getKey()]] <<- fi
    },

    addOutput = function(fi) {
      outputFileInfos[[fi$getKey()]] <<- fi
    },

    addScriptFileInfo = function(filePath) {
      fi <- FileInfo$new(init = "auto", access = "read", filePath)
      scriptFileInfo <<- fi
    },

    addScriptPackageInfo = function(envName) {
      pkgDetails <- getPackageDetails(envName)
      
      fi <- FileInfo$new(init = "manual", access = "read")

      fi$fileType <- "Package"
      fi$repoType <- "Package"
      fi$repoPath <- pkgDetails$package
      fi$repoVersion <- pkgDetails$version
      fi$repoModifiedBy <- pkgDetails$author
      fi$repoModified <- as.POSIXct(pkgDetails$built)
      fi$description <- pkgDetails$details

      rdbFilePath <- paste(.libPaths()[1], pkgDetails$package, "R",
                           paste0(pkgDetails$package, ".rdb"), sep = "/"
      )
      fi$fileHash <- getFileHash(rdbFilePath)

      scriptFileInfo <<- fi
    },

    toList = function() {
      classFields <- getRefClass()$fields()
      objFieldValues <- list()
      for (fieldName in names(classFields)) {
        if (fieldName %in% c("inputFileInfos", "outputFileInfos")) {
          objFieldValues[[fieldName]] <- names(.self[[fieldName]])
        } else if (fieldName == "scriptFileInfo") {
          if(!is.null(.self[[fieldName]])){
            objFieldValues[[fieldName]] <- .self[[fieldName]]$getKey() 
          }
        # } else if (fieldName == "superActionId") {
        #   if (is.null(.self$superAction)) { # length
        #     objFieldValues[[fieldName]] <- NaN
        #   } else {
        #     objFieldValues[[fieldName]] <- .self$superAction$actionId # [[1]]
        #   }
        } else if (fieldName == "superAction") {
          objFieldValues[[fieldName]] <- NULL # list()
        } else {
          objFieldValues[[fieldName]] <- .self[[fieldName]]
        }
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
      obj$executionStarted <- as.POSIXct(obj$executionStarted,format="%Y-%m-%dT%H:%M:%S")
      obj$executed <- as.POSIXct(obj$executed,format="%Y-%m-%dT%H:%M:%S")
      for (fieldName in names(classFields)) {
        if(!is.null(obj[[fieldName]])){
          .self[[fieldName]] <- obj[[fieldName]]
        }
      }
      return(.self)
    },

    toActionFrame = function(dbFields) {
      # Unit test
      actionFrame <- setNames(data.frame(matrix(ncol = length(dbFields), nrow = 0)), dbFields)
      actionFrame$action_id <- NULL

      classFields <- getRefClass()$fields()

      for (fieldName in names(classFields)) {
        if (fieldName %in% c("scriptFileInfo", "inputFileInfos", "outputFileInfos")) {
          next
        } else if (fieldName == "superActionId") {  # to get the DB - actionId of superAction  (previously set, see insertToDB)
          if (length(.self$superAction) == 0) {
            actionFrame[1, "super_action_id"] <- NaN
          } else {
            actionFrame[1, "super_action_id"] <- .self$superAction$actionId
          }
        } else if (fieldName == "activityId") {
          actionFrame[1, "activity_id"] <- setNAifEmpty(.self$activityId)
        } else if (fieldName == "groupId") {
          actionFrame[1, "group_id"] <- setNAifEmpty(.self$groupId)
        }
        else {
          dbFieldName <- tlconst$AI_DB_MAP[[fieldName]]
          # print(paste(fieldName, dbFieldName,collapse = ", "))
          if (!is.null(dbFieldName)) {
            if (dbFieldName %in% dbFields) {
              # Since there is only one record per action, setting the row of dataframe
              actionFrame[1, dbFieldName] <- .self[[fieldName]]
            }
          }
        }
      }

      # TODO: For some reason, the date is being stored as integer. will investigate this later
      actionFrame$executed <- as.POSIXct(actionFrame$executed, origin = "1970-01-01 UTC")

      return(actionFrame)
    },

    toActionFileFrame = function(dbFields) {
      # Unit test
      actionFileFrame <- setNames(data.frame(matrix(ncol = length(dbFields), nrow = 0)), dbFields)

      for (fieldName in c("scriptFileInfo", "inputFileInfos", "outputFileInfos")) {
        accessMode <- capStr(str_replace(fieldName, "FileInfo[s]*", ""))

        if (fieldName == "scriptFileInfo") {
          actionFileFrame <- addToActionFileFrame(actionFileFrame, .self[["scriptFileInfo"]], accessMode)
        } else {
          for (fi in .self[[fieldName]]) {
            actionFileFrame <- addToActionFileFrame(actionFileFrame, fi, accessMode)
          }
        }
      }
      return(actionFileFrame)
    },

    addToActionFileFrame = function(actionFileFrame, fi, accessMode) {
      actionFileRecord <- list()
      if (fi$repoPath == "") {
        actionFileRecord[["repo_path"]] <- fi$localFilePath
        actionFileRecord[["repo_version"]] <- "-1"
        actionFileRecord[["file_stored"]] <- FALSE
      } else {
        actionFileRecord[["repo_path"]] <- fi$repoPath
        actionFileRecord[["repo_version"]] <- fi$repoVersion
        actionFileRecord[["file_stored"]] <- TRUE
      }
      actionFileRecord[["action_id"]] <- .self$actionId
      actionFileRecord[["file_hash"]] <- fi$fileHash
      actionFileRecord[["access_mode"]] <- accessMode

      actionFileFrame <- rbind.data.frame(actionFileFrame, actionFileRecord, stringsAsFactors = FALSE)
      return(actionFileFrame)
    }
    # ,
    # JJ 2020-04-03 separate DB code
    # insertToDB = function(dbCon) {
    #   # error handling done in tSaveMetadataToDB
    #   actionFrame <- toActionFrame(dbListFields(dbCon, c(tlconst$DB_SCHEMA, "action")))
    # 
    #   dbWriteTable(dbCon, c(tlconst$DB_SCHEMA, "action"), value = actionFrame, append = TRUE, row.names = FALSE)
    #   .self$actionId <- getLastActionId(dbCon) 
    # 
    #   actionFileFrame <- toActionFileFrame(dbListFields(dbCon, c(tlconst$DB_SCHEMA, "action_file")))
    # 
    #   dbWriteTable(dbCon, c(tlconst$DB_SCHEMA, "action_file"), value = actionFileFrame, append = TRUE, row.names = FALSE)
    # }
  )
)
