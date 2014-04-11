context("add device")

sendRequest=getOption("epivizrTestSendRequest")

test_that("addDevice block works", {
	sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
  	devObj <- mgr$addDevice(gr, "ms1", sendRequest=sendRequest)
  	expect_is(devObj, "EpivizDevice")

  	msId <- devObj$getMsId()
    chartId <- devObj$getChartId()

    chartObj <- devObj$getChartObject()
    ms <- structure(devObj$getMsObject()$getName(), names=msId)
    expect_equal(chartObj$measurements, ms)
    expect_equal(chartObj$type, "blocksTrack")
    expect_false(is.null(mgr$chartList[[chartId]]))

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("addDevice bp works", {
	sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
  	devObj <- mgr$addDevice(gr, "ms1", sendRequest=sendRequest, type="bp")
  	expect_is(devObj, "EpivizDevice")

    msId <- devObj$getMsId()
    chartId <- devObj$getChartId()

    ms <- structure(paste0(devObj$getMsObject()$getName(), "$score", 1:2), names=paste0(msId,"__score",1:2))
    chartObj <- devObj$getChartObject()
    expect_equal(chartObj$measurements, ms)
    expect_equal(chartObj$type, "lineTrack")
    expect_false(is.null(mgr$chartList[[chartId]]))

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("addDevice feature works", {
	sendRequest=sendRequest
  sset <- makeSExp()
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
  	devObj <- mgr$addDevice(sset, "ms1", sendRequest=sendRequest, columns=c("A","B"), assay="counts2")
	expect_is(devObj, "EpivizDevice")

    msId <- devObj$getMsId()
    chartId <- devObj$getChartId()

    chartObj <- devObj$getChartObject()
    ms <- structure(paste0(devObj$getMsObject()$getName(), "$", c("A","B")), names=paste0(msId, "__", c("A","B")))
    expect_equal(chartObj$measurements, ms)
    expect_equal(chartObj$type, "geneScatterPlot")
    expect_false(is.null(mgr$chartList[[chartId]]))

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})
