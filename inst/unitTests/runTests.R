localTesting <- TRUE
if (require("RUnit", quietly = TRUE)) {
    require("textutils")
    if (localTesting)
        path <- "~/Packages/textutils/inst/unitTests" else
    path <- system.file("unitTests", package = "textutils")
    myTestSuite <- defineTestSuite("textutils",
                                   dirs = path,
                                   testFileRegexp = "ut_.*")
    stopifnot(isValidTestSuite(myTestSuite))
    testResult <- runTestSuite(myTestSuite, verbose = 0L)
    printTextProtocol(testResult, showDetails = TRUE,
                      fileName = paste(file.path(path, "report"),
                      ".txt", sep = ""))
}
