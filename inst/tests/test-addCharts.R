context("addCharts")

sendRequest=getOption("epivizrTestSendRequest")

test_that("blockChart works", {
	sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
    msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest)
    msId <- msObj$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    ms <- msObj$getMeasurements()
    chartObj <- mgr$blockChart(ms, sendRequest=sendRequest)
    chartId <- chartObj$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    expect_is(chartObj, "EpivizChart")
    expect_equal(chartObj$measurements, ms)
    expect_equal(chartObj$type, "epiviz.plugins.charts.BlocksTrack")
    expect_false(is.null(mgr$chartList[[chartId]]))

    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("plot block works", {
	sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
    msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest)
    msId <- msObj$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
	chartObj <- msObj$plot(sendRequest=sendRequest)
    chartId <- chartObj$getId()

  #  ms <- structure(msObj$getName(), names=msId)
    ms <- msObj$getMeasurements()
    expect_is(chartObj, "EpivizChart")
    expect_equal(chartObj$measurements, ms)
    expect_equal(chartObj$type, "epiviz.plugins.charts.BlocksTrack")
    expect_false(is.null(mgr$chartList[[chartId]]))

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("lineChart works", {
	sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
    msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest, type="bp")
    msId <- msObj$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
#    ms <- structure(paste0(msObj$getName(), "__score2"), names=paste0(msId,"__score2"))
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

test_that("plot bp works", {
	sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
    msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest, type="bp")
    msId <- msObj$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    chartObj <- msObj$plot(sendRequest=sendRequest)
    chartId <- chartObj$getId()

    ms <- structure(paste0(msObj$getName(), "$score", 1:2), names=paste0(msId,"__score",1:2))
    ms <- msObj$getMeasurements()
    expect_equal(chartObj$measurements, ms)
    expect_equal(chartObj$type, "epiviz.plugins.charts.LineTrack")
    expect_false(is.null(mgr$chartList[[chartId]]))

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("scatterChart works", {
	sendRequest=sendRequest
  sset <- makeSExp()
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
    msObj <- mgr$addMeasurements(sset, "ms1", sendRequest=sendRequest, columns=c("A","B"), assay="counts2")
    msId <- msObj$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
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

test_that("plot feature works", {
	sendRequest=sendRequest
  sset <- makeSExp()
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
    msObj <- mgr$addMeasurements(sset, "ms1", sendRequest=sendRequest, columns=c("A","B"), assay="counts2")
    msId <- msObj$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    chartObj <- msObj$plot(sendRequest=sendRequest)
    chartId <- chartObj$getId()

    ms <- structure(paste0(msObj$getName(), "$", c("A","B")), names=paste0(msId, "__", c("A","B")))
    ms <- msObj$getMeasurements()
#    print(chartObj$measurements);print(ms)
    expect_equal(chartObj$measurements, ms)
    expect_equal(chartObj$type, "epiviz.plugins.charts.ScatterPlot")
    expect_false(is.null(mgr$chartList[[chartId]]))

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

