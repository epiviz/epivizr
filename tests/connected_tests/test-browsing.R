context("browser commands")

test_that("navigate works", {
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  
  if (!.canPhantomTest()) {
    skip("This test can't be run in this environment")
  }
  
  server <- epivizrServer::createServer(port=7123L, 
                                        static_site_path=".",
                                        daemonized=TRUE, 
                                        verbose=TRUE)
  if (!server$is_daemonized()) {
    skip("This test only works for daemonized servers")
  }
  
  .startRemoteDriver()
  on.exit({cat("stopping remDr\n"); .stopPhantomJS()})
  
  server$start_server()
  on.exit({cat("stopping server\n"); server$stop_server()}, add=TRUE)
  
  chart_mgr <- EpivizChartMgr$new(server)
  data_mgr <- epivizrData::createMgr(server)
  
  .navigateRemoteDriver(port=server$.port)
  wait_until(server$is_socket_connected())
  
  app <- EpivizApp$new(server=server,
                       data_mgr=data_mgr,
                       chart_mgr=chart_mgr)
  app$navigate(chr="chr10", start=2000, end=10000)
  wait_until(!app$server$has_request_waiting())
  
  outputEl <- remDr$findElement(using="id", "navigate_output")
  obs_location <- outputEl$getElementText()[[1]]
  expect_equal(obs_location, "chr10:2000-10000")
})

test_that("slideshow works", {
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  
  if (!.canPhantomTest()) {
    skip("This test can't be run in this environment")
  }
  
  server <- epivizrServer::createServer(port=7123L, 
                                        static_site_path=".",
                                        daemonized=TRUE, 
                                        verbose=TRUE)
  if (!server$is_daemonized()) {
    skip("This test only works for daemonized servers")
  }
  
  .startRemoteDriver()
  on.exit({cat("stopping remDr\n"); .stopPhantomJS()})
  
  server$start_server()
  on.exit({cat("stopping server\n"); server$stop_server()}, add=TRUE)
  
  chart_mgr <- EpivizChartMgr$new(server)
  data_mgr <- epivizrData::createMgr(server)
  
  .navigateRemoteDriver(port=server$.port)
  wait_until(server$is_socket_connected())
  
  app <- EpivizApp$new(server=server,
    data_mgr=data_mgr,
    chart_mgr=chart_mgr)
  
  granges <- GenomicRanges::GRanges("chr10", 
    IRanges::IRanges(start=(1:10)*1000, width=10000))
  
  callback <- function(chr, start, end) {
    wait_until(!app$server$has_request_waiting())
    outputEl <- remDr$findElement(using="id", "navigate_output")
    obs_location <- outputEl$getElementText()[[1]]
    expect_equal(obs_location, sprintf("%s:%d-%d", chr, start, end))
  }
  app$slideshow(granges, n=4, .callback=callback)
})
