context("add charts")

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
  expect_true(is.null(chart_mgr$.chart_id_map[[chart_id]]))
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
  expect_true(is.null(chart_mgr$.chart_id_map[[chart_id]]))
  
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
  expect_true(is.null(chart_mgr$.chart_id_map[[chart_id]]))
  
})

test_that("lineChart works", {
  skip("for now")
	sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  seqlengths(gr) <- c("chr1"=200000)
  
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
    msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest, type="bp")
    msId <- msObj$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
#    ms <- structure(paste0(msObj$getName(), "__score2"), names=paste0(msId,"__score2"))

    if (standalone) {
      mgr$addSeqinfo(seqinfo(gr))
      if (sendRequest) wait_until(!mgr$server$requestWaiting)
  
      navigate_range <- gr[1,] + 2000      
      mgr$navigate(as.character(seqnames(navigate_range)), start(navigate_range), end(navigate_range))
      if (sendRequest) wait_until(!mgr$server$requestWaiting)
    }

    ms <- msObj$getMeasurements()[2]
    chartObj <- mgr$lineChart(ms, sendRequest=sendRequest)
    chartId <- chartObj$getId()

    expect_equal(chartObj$measurements, ms)
    expect_equal(chartObj$type, "epiviz.plugins.charts.LineTrack")
    expect_false(is.null(mgr$chartList[[chartId]]))

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("scatterChart works", {
  skip("for now")
	sendRequest=sendRequest
  sset <- makeSExp()
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
    msObj <- mgr$addMeasurements(sset, "ms1", sendRequest=sendRequest, columns=c("A","B"), assay="counts2")
    msId <- msObj$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    
    if (standalone) {
      mgr$addSeqinfo(seqinfo(sset))
      if (sendRequest) wait_until(!mgr$server$requestWaiting)
      
      navigate_range <- rowRanges(sset)[1,] + 2000
      mgr$navigate(as.character(seqnames(navigate_range)), start(navigate_range), end(navigate_range))
      if (sendRequest) wait_until(!mgr$server$requestWaiting)
    }
    
    x <- structure(paste0(msObj$getName(), "$A"), names=paste0(msId, "__A"))
    y <- structure(paste0(msObj$getName(), "$B"), names=paste0(msId, "__B"))
    ms <- msObj$getMeasurements()
    chartObj <- mgr$scatterChart(x=ms[[1]], y=ms[[2]],sendRequest=sendRequest)
    chartId <- chartObj$getId()

    expect_equal(chartObj$measurements, ms)
    expect_equal(chartObj$type, "epiviz.plugins.charts.ScatterPlot")
    expect_false(is.null(mgr$chartList[[chartId]]))

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

