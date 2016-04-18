context("object creation")

openBrowser=sendRequest

test_that("stop shuts down the server connection", {
  mgr=.startMGR(openBrowser=openBrowser)
  expect_equal(mgr$isClosed(), !openBrowser)
  
  mgr$stopServer()
  expect_true(mgr$isClosed())
})

test_that("startEpiviz creates a proper object", {
  mgr <- .startMGR(openBrowser)
  expect_is(mgr, "EpivizDeviceMgr")
  
  expect_is(mgr$msList, "list")
  expect_equal(length(mgr$msList), 3)
  
  expect_is(mgr$msList$gene, "list")
  expect_equal(length(mgr$msList$gene), 0)
  
  expect_is(mgr$msList$bp, "list")
  expect_equal(length(mgr$msList$bp), 0)
  
  expect_is(mgr$msList$block, "list")
  expect_equal(length(mgr$msList$block), 0)
  
  expect_is(mgr$server, "EpivizServer")
  expect_equal(mgr$chartIdMap, list())
  
  expect_equal(mgr$isClosed(), !openBrowser)
  mgr$stopServer()
})

test_that("nice error message shows up and no browser window is opened", {
  mgr <- .startMGR(openBrowser)

  if (!openBrowser) {
    mgr$startServer()
  }

  expect_error(tryCatch({
    mgr2 <- .startMGR(openBrowser)
    if (!openBrowser) {
      mgr2$startServer()
    }
  }, error=function(e) {print(e); stop(e)}))
  mgr$stopServer()
})