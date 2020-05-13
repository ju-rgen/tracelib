# test_file("tests/testthat/test-workflow.R", reporter = RstudioReporter)
options(keep.source = TRUE)
library(jsonlite)

test_that("test workflow with helper", {
  temp_dir <- prepareTempDataDir("../data")
  expect_silent(expect_null(workflow(storageMode = "File", outputFolder=temp_dir)))
  outputFile <- paste(temp_dir, "test_workflow.json", sep="/")
  expect_true(file.exists(outputFile)) 
  expect_silent(jsonData <- read_json(outputFile))
  expect_length(jsonData$actionInfos, 2)
  expect_length(jsonData$fileInfos, 3)
  expect_length(jsonData$systemInfo, 1)
  #print(jsonData)
  
  #testCallstack()
})

