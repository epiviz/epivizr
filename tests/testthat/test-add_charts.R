context("disconnected add charts")

test_that("adding a block chart works using visualize with just measurement", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  
  gr <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=1))
  ms_obj <- data_mgr$add_measurements(gr, "ms1", send_request=FALSE)
  ms_id <- ms_obj$get_id()
  ms <- ms_obj$get_measurements()

  chart_mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")
  chart_obj <- chart_mgr$visualize("BlockChart", measurements=ms)
  chart_id <- chart_obj$get_id()
  
  expect_is(chart_obj, "EpivizChart")
  expect_equal(chart_obj$.measurements, ms)
  expect_equal(chart_obj$.type, "epiviz.plugins.charts.BlocksTrack")
  expect_true(exists(chart_id, env=chart_mgr$.chart_list))
  expect_equal(chart_obj$get_app_id(), character())
})

test_that("adding a block chart works using visualize with just datasource", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  
  gr <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=1))
  ms_obj <- data_mgr$add_measurements(gr, "ms1", send_request=FALSE)
  ms_id <- ms_obj$get_id()
  ms <- ms_obj$get_measurements()
  
  chart_mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")
  chart_obj <- chart_mgr$visualize("BlockChart", datasource=ms_obj)
  chart_id <- chart_obj$get_id()
  
  expect_is(chart_obj, "EpivizChart")
  expect_equal(chart_obj$.measurements, ms)
  expect_equal(chart_obj$.type, "epiviz.plugins.charts.BlocksTrack")
  expect_true(exists(chart_id, env=chart_mgr$.chart_list))
  expect_equal(chart_obj$get_app_id(), character())  
})

test_that("adding a block chart works using visualize with both arguments", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  
  gr <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=1))
  ms_obj <- data_mgr$add_measurements(gr, "ms1", send_request=FALSE)
  ms_id <- ms_obj$get_id()
  ms <- ms_obj$get_measurements()
  
  chart_mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")
  chart_obj <- chart_mgr$visualize("BlockChart", datasource=ms_obj)
  chart_id <- chart_obj$get_id()

  chart_obj <- chart_mgr$visualize("BlockChart", measurements=ms, datasource=ms_obj)
  chart_id <- chart_obj$get_id()
  
  expect_is(chart_obj, "EpivizChart")
  expect_equal(chart_obj$.measurements, ms)
  expect_equal(chart_obj$.type, "epiviz.plugins.charts.BlocksTrack")
  expect_true(exists(chart_id, env=chart_mgr$.chart_list))
  expect_equal(chart_obj$get_app_id(), character())
})

test_that("adding a line chart works using visualize with just measurement", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  
  gr <- GenomicRanges::GRanges(seqnames="chr1", 
    ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),
    score2=rnorm(length(seq(1,100,by=25))))

  ms_obj <- data_mgr$add_measurements(gr, "ms1", type="bp", send_request=FALSE)
  ms_id <- ms_obj$get_id()
  ms <- ms_obj$get_measurements()[2]
  
  chart_mgr$register_chart_type("LineChart", "epiviz.plugins.charts.LineTrack")
  chart_obj <- chart_mgr$visualize("LineChart", measurements=ms)
  chart_id <- chart_obj$get_id()

  expect_is(chart_obj, "EpivizChart")
  expect_equal(chart_obj$.measurements, ms)
  expect_equal(chart_obj$.type, "epiviz.plugins.charts.LineTrack")
  expect_true(exists(chart_id, env=chart_mgr$.chart_list))
  expect_equal(chart_obj$get_app_id(), character())
})

test_that("adding a line chart works using visualize with just datasource", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  
  gr <- GenomicRanges::GRanges(seqnames="chr1", 
                               ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), 
                               score1=rnorm(length(seq(1,100,by=25))),
                               score2=rnorm(length(seq(1,100,by=25))))
  
  ms_obj <- data_mgr$add_measurements(gr, "ms1", type="bp", send_request=FALSE)
  ms_id <- ms_obj$get_id()
  ms <- ms_obj$get_measurements()[2]
  
  chart_mgr$register_chart_type("LineChart", "epiviz.plugins.charts.LineTrack")
  chart_obj <- chart_mgr$visualize("LineChart", datasource=ms_obj)
  chart_id <- chart_obj$get_id()
  
  expect_is(chart_obj, "EpivizChart")
  expect_equal(chart_obj$.measurements, ms_obj$get_measurements())
  expect_equal(chart_obj$.type, "epiviz.plugins.charts.LineTrack")
  expect_true(exists(chart_id, env=chart_mgr$.chart_list))
  expect_equal(chart_obj$get_app_id(), character())
})

test_that("adding a line chart works using visualize with both arguments", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  
  gr <- GenomicRanges::GRanges(seqnames="chr1", 
                               ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), 
                               score1=rnorm(length(seq(1,100,by=25))),
                               score2=rnorm(length(seq(1,100,by=25))))
  
  ms_obj <- data_mgr$add_measurements(gr, "ms1", type="bp", send_request=FALSE)
  ms_id <- ms_obj$get_id()
  ms <- ms_obj$get_measurements()[2]
  
  chart_mgr$register_chart_type("LineChart", "epiviz.plugins.charts.LineTrack")
  chart_obj <- chart_mgr$visualize("LineChart", measurements=ms, datasource=ms_obj)
  chart_id <- chart_obj$get_id()
  
  expect_is(chart_obj, "EpivizChart")
  expect_equal(chart_obj$.measurements, ms)
  expect_equal(chart_obj$.type, "epiviz.plugins.charts.LineTrack")
  expect_true(exists(chart_id, env=chart_mgr$.chart_list))
  expect_equal(chart_obj$get_app_id(), character())
})

