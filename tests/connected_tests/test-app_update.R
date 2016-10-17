context("connected app data update")

test_that("connected data update through app works", {
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
  
  gr <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=1))
  gr2 <- GenomicRanges::GRanges(seqnames="chr12", ranges=IRanges::IRanges(start=1:1000,width=10))
  
  chart_mgr$register_chart_type("BlocksTrack")
  chart_obj <- app$plot(gr, datasource_name="ms1")
  chart_id <- chart_obj$get_id()
  wait_until(!server$has_request_waiting())
  Sys.sleep(1)
  
  ms_obj <- app$get_ms_object(chart_obj)
  app$update_measurements(ms_obj, gr2)
  wait_until(!server$has_request_waiting())
  Sys.sleep(1)

  outEl <- remDr$findElement(using="id", "clear_cache_output")
  res <- outEl$getElementText()[[1]]
  expect_equal(res, paste(ms_obj$get_id(), "cache cleared."))
  
  outputEl <- remDr$findElement(using="id", "redraw_output")
  out_text <- outputEl$getElementText()[[1]]
  expect_equal(out_text, "redraw called")
})