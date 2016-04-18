context("connected app plot methods")

test_that("plot feature works", {
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
  se <- make_test_SE()

  chart_mgr$register_chart_type("ScatterPlot", "epiviz.plugins.charts.ScatterPlot")    
  chart_obj <- app$plot(se, "ms1", columns=c("A","B"), assay="counts2")
  chart_id <- chart_obj$get_id()
  wait_until(!server$has_request_waiting())
  Sys.sleep(2)
  
  expect_true(chart_obj$is_connected())
  
  ms_obj <- app$get_ms_object(chart_id) 
  ms_id <- ms_obj$get_id()
  ms <- ms_obj$get_measurements()

  outputEl <- remDr$findElement(using="id", "add_chart_output")
  ms_list <- outputEl$getElementText()[[1]]
  exp_list <- paste0(sprintf("%s:%s", ms_id, c("A","B")), collapse=",")
  exp_list <- paste0("epiviz.plugins.charts.ScatterPlot:", chart_obj$.app_id, "[", exp_list, "]")
  expect_equal(ms_list, exp_list)
})

