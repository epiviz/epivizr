context("restart epiviz")

test_that("restartEpiviz restarts connection and workspace from file", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  app <- EpivizApp$new(server=server,
                       data_mgr=data_mgr,
                       chart_mgr=chart_mgr)
  
  gr1 <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=100))
  gr2 <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  se <- make_test_SE()
  
  dev1 <- app$data_mgr$add_measurements(gr1, "dev1", send_request=FALSE)
  dev2 <- app$data_mgr$add_measurements(gr2, "dev2", send_request=FALSE, type="bp")
  dev3 <- app$data_mgr$add_measurements(se, "dev3", send_request=FALSE, columns=c("A", "B"))
  
  app$chart_mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")
  app$chart_mgr$register_chart_type("LineChart", "epiviz.plugins.charts.LineTrack")
  app$chart_mgr$register_chart_type("ScatterChart", "epiviz.plugins.charts.ScatterChart")
  
  chart1 <- app$chart_mgr$visualize("BlockChart", datasource=dev1)
  chart2 <- app$chart_mgr$visualize("LineChart", datasource=dev2)
  chart3 <- app$chart_mgr$visualize("ScatterChart", datasource=dev3)
  
  file_name <- tempfile(fileext=".rda")
  
  #expect_false(app$server$is_closed())
  expect_false(file.exists(file_name))
  
  app$save(file=file_name, include_data=TRUE)
  app$stop_app()
  
  expect_true(file.exists(file_name))
  
  app2 <- restartEpiviz(file_name, open_browser=FALSE)
  
  expect_is(app2, "EpivizApp")
  expect_is(app2$server, "EpivizServer")
  expect_is(app2$chart_mgr, "EpivizChartMgr")
  expect_is(app2$data_mgr, "EpivizDataMgr")

  expect_equal(app2$chart_mgr$num_charts(), 3)  
  
  #app$stop_app()
  #file.remove(file_name)
})

test_that("restartEpiviz restarts connection and workspace from environment", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  app <- EpivizApp$new(server=server,
                       data_mgr=data_mgr,
                       chart_mgr=chart_mgr)
  
  
  gr1 <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=100))
  gr2 <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  se <- make_test_SE()
  
  dev1 <- app$data_mgr$add_measurements(gr1, "dev1", send_request=FALSE)
  dev2 <- app$data_mgr$add_measurements(gr2, "dev2", send_request=FALSE, type="bp")
  dev3 <- app$data_mgr$add_measurements(se, "dev3", send_request=FALSE, columns=c("A", "B"))
  
  app$chart_mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")
  app$chart_mgr$register_chart_type("LineChart", "epiviz.plugins.charts.LineTrack")
  app$chart_mgr$register_chart_type("ScatterChart", "epiviz.plugins.charts.ScatterChart")
  
  chart1 <- app$chart_mgr$visualize("BlockChart", datasource=dev1)
  chart2 <- app$chart_mgr$visualize("LineChart", datasource=dev2)
  chart3 <- app$chart_mgr$visualize("ScatterChart", datasource=dev3)
  
  file_name <- tempfile(fileext=".rda")
  
  #expect_false(app$server$is_closed())
  expect_false(file.exists(file_name))
  
  app$save(file=file_name,include_data=FALSE)
  app$stop_app()
  
  expect_true(file.exists(file_name))
  #expect_true(app$server$is_closed())
  
  app2 <- restartEpiviz(file_name, open_browser=FALSE)
  
  #expect_false(app$server$is_closed())
  expect_is(app2, "EpivizApp")
  expect_is(app2$server, "EpivizServer")
  expect_is(app2$chart_mgr, "EpivizChartMgr")
  expect_is(app2$data_mgr, "EpivizDataMgr")
  
  expect_equal(app2$chart_mgr$num_charts(), 3)  
  
  chart_ids <- ls(envir=app$chart_mgr$.chart_list)
  chart_source_names <- lapply(chart_ids, function(id){
    chart_obj <- app$chart_mgr$.get_chart_object(id)
    chart_source <- chart_obj$get_source_name()
  })
  
  datasource_names <- ls(app$data_mgr$.ms_list)
  data_mgr_origin_names <- lapply(datasource_names, function(name) {
    ms_obj <- app$data_mgr$.get_ms_object(name);
    ms_obj$get_source_name()
  })
  
  for (chart_source_name in chart_source_names){
    expect_true(chart_source_name %in% data_mgr_origin_names)
  }

  #app$stop_app()
  #file.remove(file_name)
})
