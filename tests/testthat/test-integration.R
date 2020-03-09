# test_that("testing writing metadata to json file using testtracelib workflow", {
#    #  library(tracelib)
#    # TODO: removed dependency tracelib from testtracelib, otherwise problem here - better solution required!
#    library(testtracelib)
#    filename <- paste(format(Sys.time(), "%Y_%m_%d_%H_%M"), "json", sep = ".")
#    CALLSTACKLOGDIR <<- "C:/Projects/Git/tracelib/tests/data/"
#    filePath <- paste0(CALLSTACKLOGDIR, "input1.csv")
#    workflowRead1File(jsonFilePath = CALLSTACKLOGDIR, filePath = filePath)
#   expect_true(file.exists(filePath))
#  })
# 
# 
#  test_that("testing writing metadata to json file using tracelib/tests workflow", {
#    #  library(tracelib)
#    # TODO: removed dependency tracelib from testtracelib, otherwise problem here - better solution required!
#    library(testtracelib)
# 
#    filename <- paste(format(Sys.time(), "%Y_%m_%d_%H_%M"), "json", sep = ".")
#    CALLSTACKLOGDIR <<- "C:/Projects/Git/tracelib/tests/data/"
#    filePath <- paste0(CALLSTACKLOGDIR, "input1.csv")
#    workflow(jsonFilePath = CALLSTACKLOGDIR, filePath = filePath)
#    expect_true(file.exists(filePath))
#  })