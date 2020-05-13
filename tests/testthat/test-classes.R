## Test runnning funcitons from packages and scripts
## Test read and writing to json files
## Test DB access


test_that("testing file info class", {
  #initGlobals()
  fi <- FileInfo$new(init = "auto", access = "read", filePath = "data/input1.csv")
  json <- fi$toJSONObject()
  df <- fi$addToFrame()
  
  # expect_warning(fi)
  expect_is(fi, "FileInfo")
  expect_equal(fi$fileHash,"md5:NOHASH")
  expect_equal(fi$repoType,"Local")
  expect_match(fi$repoPath,".*#data/input1.csv")
  expect_equal(nchar(fi$repoVersion), 13)
  expect_equal(fi$repoModified,tlconst$OT)
  expect_equal(fi$accessInfo,"read")
  expect_equal(fi$fileType,"Data")
  
  expect_length(fi$toList(), 19)
  expect_is(json, "json")
  expect_equal(nrow(df),1)
  expect_equal(ncol(df),19)
  
  #Write DF and compare 
  
})

test_that("testing system info class", {
  si <- SystemInfo$new()
  json <- si$toJSONObject()
  df <- si$addToFrame()
  info <- Sys.info()
  
  expect_is(si, "SystemInfo")
  expect_equal(si$systemId,1)
  expect_equal(si$systemName,info[["nodename"]])
  expect_equal(si$operatingSystem,info[["sysname"]])
  expect_equal(si$osVersion,info[["release"]])
  expect_equal(si$RVersion,paste(R.version$major, R.version$minor, sep = "."))
  expect_false(si$validated)
  expect_is(json, "json")
  expect_length(si$toList(), 7)
  expect_equal(nrow(df),1)
  expect_equal(ncol(df),7)
  
  #Compare current system info
})


test_that("testing action info initialize and tStartAction", {

  workflow(storageMode="File", outputFolder = "../data/")

  ai1 <- tlvar$ACTION_INFOS[[1]]

  expect_length(ai1$toList(),12)
  expect_is(ai1, "ActionInfo")
  expect_equal(ai1$actionName, "workflow")
  expect_equal(ai1$executedBy, Sys.info()[["user"]] )
  expect_equal(ai1$actionType,"ImportFile")
  expect_equal(ai1$actionId,1)
  expect_equal(ai1$systemId,1)
  expect_equal(ai1$groupId,"1")
  expect_null(ai1$superAction)
  expect_equal(ai1$superActionId,NaN)
  expect_identical(as.Date(ai1$executed),Sys.Date())
  expect_identical(as.Date(ai1$executionStarted),Sys.Date())
  #expect_identical(ai1$description,"R_GlobalEnv:workflow/tracelib:tStartMetadataCapture")
  
  ai2 <- tlvar$ACTION_INFOS[[2]]
  
  expect_length(ai2$toList(),14) # inputFileInfos and outputFileInfos added to workflow

  expect_is(ai2, "ActionInfo")
  expect_is(ai2$superAction,"ActionInfo")
  expect_equal(ai2$actionName, "call_action")
  expect_equal(ai2$executedBy, Sys.info()[["user"]] )
  expect_equal(ai2$actionType,"Other")
  expect_equal(ai2$actionId,2)
  expect_equal(ai2$systemId,1)
  expect_equal(ai2$groupId,"1")
  expect_equal(ai2$superAction,ai1)
  expect_equal(ai2$superActionId,1)
  expect_identical(as.Date(ai2$executed),Sys.Date())
  expect_identical(as.Date(ai2$executionStarted),Sys.Date())
  #expect_identical(ai2$description,"R_GlobalEnv:workflow/R_GlobalEnv:call_action/tracelib:tStartAction")
  
})

test_that("testing action info  toJsonObject, jsonObj -> file, file -> jsonObj, jsonObj -> actionInfo2", {
  
  ai <- initAi()
  expect_is(ai, "ActionInfo")
  
  aiJSON <- ai$toJSONObject()
  writeJsonObjectToFile(aiJSON,
                        outputFolder = "../data/",
                        jsonFileName = "test_action",
                        useTopLevelAction = F,useTimeStamp = F)
  
  ai2 <- ActionInfo$new(init="manual")
  ai2$fromList(fromJSON("../data/test_action.json"))
  # ai2 <- ai2$fromList(fromJSON("../data/test_action.json"))
  
  expect_is(ai2, "ActionInfo")
  expect_equal(ai,ai2)
  
})
 
 

# test_that("testing action and action file frame generation for action class", {
#  
# workflow(storageMode="File", outputFolder = "../data/")
#   ai <- tlvar$ACTION_INFOS[[1]]
#   
#   expect_is(ai, "ActionInfo")
#   dbFields <- readRDS("../data/dbFields.rds")
#   
#     #Create DB list fields and use that as ref frame
#   af <- ai$toActionFrame(dbFields$action)
#   aff <- ai$toActionFileFrame(dbFields$actionFile)
# 
#   expect_equal(nrow(af),1)
#   expect_equal(nrow(aff),1)
# 
#  store the right result data frame once, check it manually and compare with the dataframes produced
# })
 
