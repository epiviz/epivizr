context("connected manage charts")

test_that("rm_chart works using chart object", {
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
  
  gr <- GenomicRanges::GRanges(seqnames="chr1", 
    ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),
    score2=rnorm(length(seq(1,100,by=25))))

  ms_obj <- data_mgr$add_measurements(gr, "ms1", type="bp")
  ms_id <- ms_obj$get_id()
  wait_until(!server$has_request_waiting())
  
  chart_mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")
  chart_obj <- chart_mgr$visualize("BlockChart", datasource=ms_obj)
  chart_id <- chart_obj$get_id()
  wait_until(!server$has_request_waiting())
  Sys.sleep(2)

  chart_mgr$rm_chart(chart_obj)
  wait_until(!server$has_request_waiting())
  Sys.sleep(2)
  
  outputEl <- remDr$findElement(using="id", "add_chart_output")
  ms_list <- outputEl$getElementText()[[1]]
  expect_equal(ms_list, "")
})

test_that("rm_all_charts works", {
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

  gr1 <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=100))
  gr2 <- GenomicRanges::GRanges(seqnames="chr2", ranges=IRanges::IRanges(start=2:20, width=100))
  gr3 <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  se <- make_test_SE()
  
  dev1 <- data_mgr$add_measurements(gr1, "dev1"); devId1 <- dev1$get_id()
  dev2 <- data_mgr$add_measurements(gr2, "dev2"); devId2 <- dev2$get_id()
  dev3 <- data_mgr$add_measurements(gr3, "dev3", type="bp"); devId3 <- dev3$get_id()
  dev4 <- data_mgr$add_measurements(se, "dev4", columns=c("A", "B")); devId4 <- dev4$get_id()
  wait_until(!server$has_request_waiting())
  
  chart_mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")
  chart_mgr$register_chart_type("LineChart", "epiviz.plugins.charts.LineTrack")
  chart_mgr$register_chart_type("ScatterChart", "epiviz.plugins.charts.ScatterChart")
  
  chart1 <- chart_mgr$visualize("BlockChart", datasource=dev1)
  chart2 <- chart_mgr$visualize("BlockChart", datasource=dev2)
  chart3 <- chart_mgr$visualize("LineChart", datasource=dev3)
  chart4 <- chart_mgr$visualize("ScatterChart", datasource=dev4)
  wait_until(!server$has_request_waiting())

  expect_true(chart1$is_connected())
  expect_true(chart2$is_connected())
  expect_true(chart3$is_connected())
  expect_true(chart4$is_connected())

  outputEl <- remDr$findElement(using="id", "add_chart_output")
  ms_list <- outputEl$getElementText()[[1]]
  
  exp_list <- paste0("epiviz.plugins.charts.BlocksTrack:", chart1$.app_id, "[", devId1, ":", devId1, "],")
  exp_list <- paste0(exp_list,
                     "epiviz.plugins.charts.BlocksTrack:", chart2$.app_id, "[", devId2, ":", devId2, "],")
  exp_list <- paste0(exp_list,
                     "epiviz.plugins.charts.LineTrack:", chart3$.app_id, "[", devId3, ":score],")
  
  tmp <- paste0(sprintf("%s:%s", devId4, c("A","B")), collapse=",")
  tmp <- paste0("epiviz.plugins.charts.ScatterChart:", chart4$.app_id, "[", tmp, "]")
  exp_list <- paste0(exp_list, tmp)
  expect_equal(ms_list, exp_list)
  
  chart_mgr$rm_all_charts()
  wait_until(!server$has_request_waiting())
  Sys.sleep(2)
  
  outputEl <- remDr$findElement(using="id", "add_chart_output")
  ms_list <- outputEl$getElementText()[[1]]
  expect_equal(ms_list, "")
})

test_that("list_charts works", {
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
  
  gr1 <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=100))
  gr2 <- GenomicRanges::GRanges(seqnames="chr2", ranges=IRanges::IRanges(start=2:20, width=100))
  gr3 <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  se <- make_test_SE()
  
  dev1 <- data_mgr$add_measurements(gr1, "dev1"); devId1 <- dev1$get_id()
  dev2 <- data_mgr$add_measurements(gr2, "dev2"); devId2 <- dev2$get_id()
  dev3 <- data_mgr$add_measurements(gr3, "dev3", type="bp"); devId3 <- dev3$get_id()
  dev4 <- data_mgr$add_measurements(se, "dev4", columns=c("A", "B")); devId4 <- dev4$get_id()
  wait_until(!server$has_request_waiting())
  
  chart_mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")
  chart_mgr$register_chart_type("LineChart", "epiviz.plugins.charts.LineTrack")
  chart_mgr$register_chart_type("ScatterPlot", "epiviz.plugins.charts.ScatterPlot")
  
  chart1 <- chart_mgr$visualize("BlockChart", datasource=dev1)
  chart2 <- chart_mgr$visualize("BlockChart", datasource=dev2)
  chart3 <- chart_mgr$visualize("LineChart", datasource=dev3)
  chart4 <- chart_mgr$visualize("ScatterPlot", datasource=dev4)
  wait_until(!server$has_request_waiting())
  
  charts_list <- chart_mgr$list_charts()
    
  ids <- c(chart1$get_id(), chart2$get_id(), chart3$get_id(), chart4$get_id())
  type <- c("epiviz.plugins.charts.BlocksTrack",
            "epiviz.plugins.charts.BlocksTrack",
            "epiviz.plugins.charts.LineTrack",
            "epiviz.plugins.charts.ScatterPlot")
    ms <- c(paste0(dev1$get_id(),":",dev1$get_name()),
            paste0(dev2$get_id(),":",dev2$get_name()), 
            paste0(dev3$get_id(), ":score"),
            paste0(dev4$get_id(), ":", c("A","B"), collapse=","))
    connected <- rep("*", 4)
    expected_df <- data.frame(id=ids,
                              type=type,
                              measurements=ms,
                              connected=connected,
                              stringsAsFactors=FALSE)
    expect_equal(charts_list, expected_df)
})

