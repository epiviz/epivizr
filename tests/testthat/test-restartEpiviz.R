context("restart epiviz")

test_that("restartEpiviz restarts connection and workspace", {
  app <- startEpiviz(non_interactive=TRUE)
  app$server$start_server()
  
  gr1 <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=100))
  gr2 <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  se <- make_test_SE()
  
  dev1 <- app$data_mgr$add_measurements(gr1, "dev1", send_request=FALSE); devId1 <- dev1$get_id()
  dev2 <- app$data_mgr$add_measurements(gr2, "dev2", send_request=FALSE, type="bp"); devId2 <- dev2$get_id()
  dev3 <- app$data_mgr$add_measurements(se, "dev3", send_request=FALSE, columns=c("A", "B")); devId3 <- dev3$get_id()
  
  app$chart_mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")
  app$chart_mgr$register_chart_type("LineChart", "epiviz.plugins.charts.LineTrack")
  app$chart_mgr$register_chart_type("ScatterChart", "epiviz.plugins.charts.ScatterChart")
  
  chart1 <- app$chart_mgr$visualize("BlockChart", datasource=dev1)
  chart2 <- app$chart_mgr$visualize("LineChart", datasource=dev2)
  chart3 <- app$chart_mgr$visualize("ScatterChart", datasource=dev3)
  
  file_name <- "test-restartEpiviz.rda"
  
  expect_false(app$server$is_closed())
  expect_false(file.exists(file_name))
  
  app$save(file=file_name,include_data=TRUE)

  expect_true(file.exists(file_name))
  expect_true(app$server$is_closed())
  expect_equal(app$chart_mgr$num_charts(), 0)  
  
  app <- restartEpiviz(file_name, open_browser=FALSE)

  expect_false(app$server$is_closed())
  expect_is(app, "EpivizApp")
  expect_is(app$server, "EpivizServer")
  expect_is(app$chart_mgr, "EpivizChartMgr")
  expect_is(app$data_mgr, "EpivizDataMgr")

  expect_equal(app$chart_mgr$num_charts(), 3)  
  
  app$stop_app()
  file.remove(file_name)
})

# TODO: Add test for restarting epiviz and pulling data from global environment
