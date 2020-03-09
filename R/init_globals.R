
# .onLoad <- function(libname, pkgname)
# {
#   assign("tlconst", new.env())
#   assign("tlvar", new.env())
# }

tlconst <- new.env(parent = emptyenv())
tlvar <- new.env(parent = emptyenv())

#' Title
#'
#' @return
#' @export
#'
#' @examples
initConst <- function() {
  #assign("tl_test", "ex", envir = tlconst)

  #' %ni%' <- Negate('%in%')

  tlconst$FILE_TYPE_LIST <- list()

  tlconst$OT <- as.POSIXct(0, origin = "1970-01-01 UTC")
  
  tlconst$DB_SCHEMA <- "flow_trace_dev"

  tlconst$FI_DB_MAP <- list(
    fileHash = "file_hash",
    repoPath = "repo_path",
    repoVersion = "repo_version",
    repoFileStatus = "repo_file_status",
    repoModified = "repo_modified",
    repoModifiedBy = "repo_modifiedby",
    localFilePath = "local_file_path",
    localModified = "local_modified",
    localModifiedBy = "local_modifiedby",
    fileType = "file_type",
    groupId = "group_id",
    activityId = "activity_id",
    qualityChecked = "quality_checked",
    qualityComputed = "quality_computed",
    description = "description",
    repoType = "repo_type"
  )

  tlconst$AI_DB_MAP <- list(
    # actionId = "action_id",
    actionName = "action_name",
    actionType = "action_type",
    superActionId = "super_action_id",
    executed = "executed",
    executedBy = "executedby",
    activityId = "activity_id",
    systemId = "system_id",
    groupId = "group_id",
    description = "description"
  )

  tlconst$SI_DB_MAP <- list(
    systemName = "system_name",
    operatingSystem = "operating_system",
    osVersion = "operating_system_version",
    RVersion = "r_version",
    validated = "validated",
    description = "description"
  )

  fTypePath <- system.file("extdata", "file_type_suffixes.csv", package = "tracelib", mustWork = T)
  tlconst$FILE_TYPE_LIST <- prepareFileTypeConfigGlist(fTypePath)
}
