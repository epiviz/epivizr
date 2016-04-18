context("object creation")

test_that("EpivizChartMgr creates a proper object", {
  server <- epivizrServer::createServer()
  mgr <- EpivizChartMgr$new(server)
  
  expect_is(mgr, "EpivizChartMgr")
  
  expect_is(mgr$.chart_list, "environment")
  expect_equal(mgr$num_charts(), 0)
  expect_equal(mgr$.chart_idCounter, 0)
  expect_is(mgr$.server, "EpivizServer")
  expect_true(mgr$is_server_closed())
})

test_that("server opening works as expected", {
  server <- epivizrServer::createServer()
  mgr <- EpivizChartMgr$new(server)
  expect_true(mgr$is_server_closed())
  
  server$start_server()
  on.exit(server$stop_server())
  expect_false(mgr$is_server_closed())
})
test_that("stop shuts down the server connection", {
  skip("for now")
  mgr=.startMGR(openBrowser=openBrowser)
  expect_equal(mgr$isClosed(), !openBrowser)
  
  mgr$stopServer()
  expect_true(mgr$isClosed())
})

test_that("startEpiviz creates a proper object", {
  skip("for now")
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

