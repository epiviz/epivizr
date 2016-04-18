context("object creation")

test_that("EpivizChartMgr creates a proper object", {
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  
  if (!.canPhantomTest()) {
    skip("This test can't be run in this environment.")
  }
  
  server <- epivizrServer::createServer(port=7123L,
                                        static_site_path=".",
                                        daemonized=TRUE,
                                        verbose=TRUE)
  if (!server$is_daemonized()) {
    skip("This test can only run for daemonized servers.")
  }
  
  .startRemoteDriver()
  on.exit(.stopPhantomJS())
  
  server$start_server()
  on.exit(server$stop_server(), add=TRUE)
  
  mgr <- EpivizChartMgr$new(server)
  .navigateRemoteDriver(port=server$.port)
  wait_until(mgr$.server$is_socket_connected())
  
  title <- remDr$getTitle()[[1]]
  expect_equal(title, "EpivizServer Test Page")
  remDr$close()
  expect_false(mgr$is_server_closed())
})

test_that("EpivizApp works as expected", {
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  
  if (!.canPhantomTest()) {
    skip("This test can't be run in this environment.")
  }
  
  server <- epivizrServer::createServer(port=7123L,
                                        static_site_path=".",
                                        daemonized=TRUE,
                                        verbose=TRUE)
  if (!server$is_daemonized()) {
    skip("This test can only run for daemonized servers.")
  }
  
  .startRemoteDriver()
  on.exit(.stopPhantomJS())
  
  server$start_server()
  on.exit(server$stop_server(), add=TRUE)
  
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  app <- EpivizApp$new(server=server,
                       data_mgr=data_mgr,
                       chart_mgr=chart_mgr)
  
  .navigateRemoteDriver(port=server$.port)
  wait_until(app$server$is_socket_connected())
  
  title <- remDr$getTitle()[[1]]
  expect_equal(title, "EpivizServer Test Page")
  
  expect_false(app$server$is_closed())
  remDr$close()
})
