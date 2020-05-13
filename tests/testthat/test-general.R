testDataFolder <- "../data/"

test_that("get file hash", {
  fHash <- getFileHash(paste0(testDataFolder, "input1.csv"))
  expect_length(fHash, 1)
  expect_equal(fHash, "71b2bbc467b18bb4ba0dab9246ed1b8f")
})

test_that("get file type", {
  initConst()
  fType <- getFileType(paste0(testDataFolder, "input1.csv"))
  expect_equal(fType, "Data")
})

test_that("get SVN info", {
  expect_true(file.exists(paste0(testDataFolder, "input1.csv")))
  expect_null(getSVNInfo(paste0(testDataFolder, "input1.csv")))
  expect_equal(getSVNStatus(paste0(testDataFolder, "input1.csv")), "-N") # default is READ_SVN_INFO=FALSE
})

# test_that("check DB connection", {
#   # credPath <- system.file("keys.json", package = "tracelib", mustWork = T)
#   # # print(credPath)
#   # # dbCon <- getDBCon(credPath)
#   # # print(dbCon)
#   # cred <- fromJSON(credPath)
#   # # Check DB if key exists
#   # dbCon <- dbConnect(
#   #   drv = cred$drv,
#   #   host = cred$host,
#   #   user = cred$user,
#   #   password = cred$password,
#   #   dbname = cred$dbname
#   # )
#   dbCon <- getDBCon("keys.json")
#   
#   expect_true(dbExistsTable(dbCon, c("flow_trace", "action")))
# 
#   dbDisconnect(dbCon)
# })
# 
# 
# test_that("Get activity ID for repo path",{
# 
#   dbCon <- getDBCon("keys.json")
#   
#   activityId <- getActivityIdForRepoPath(dbCon, "https://by-spmsvnprd/svn/traceLib/trunk/tests/")
#   expect_equal(activityId,"0")
#   dbDisconnect(dbCon)
# })