context("manage charts")

sendRequest = .epivizrTestOpts$sendRequest

test_that("rmChart works", {
  sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  
  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    msObj <- mgr$addMeasurements(gr, "dev1", sendRequest=sendRequest, type="bp")
    msId <- msObj$getId()

    chartObj <- msObj$plot(sendRequest=sendRequest)
    chartId <- chartObj$getId()

    mgr$rmChart(chartObj)
    
    expect_equal(length(mgr$chartList), 0)
    expect_true(is.null(mgr$chartList[[chartId]]))

    chartObj <- msObj$plot(sendRequest=sendRequest)
    chartId <- chartObj$getId()

    mgr$rmChart(chartId)
    expect_equal(length(mgr$chartList), 0)
    expect_true(is.null(mgr$chartList[[chartId]]))
  },finally=mgr$stopServer())
})

test_that("listCharts works", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  eset <- makeEset()

  mgr <- .startMGR(openBrowser=sendRequest)
  tryCatch({
    dev1 <- mgr$addMeasurements(gr1, "dev1", sendRequest=sendRequest); devId1=dev1$getId()
    dev2 <- mgr$addMeasurements(gr2, "dev2", sendRequest=sendRequest); devId2=dev2$getId()
    dev3 <- mgr$addMeasurements(gr3, "dev3", sendRequest=sendRequest, type="bp"); devId3=dev3$getId()
    dev4 <- mgr$addMeasurements(eset, "dev4", sendRequest = sendRequest, columns=c("SAMP_1", "SAMP_2")); devId4=dev4$getId()
    
    chart1 <- dev1$plot(sendRequest=sendRequest)
    chart2 <- dev2$plot(sendRequest=sendRequest)
    chart3 <- dev3$plot(sendRequest=sendRequest)
    chart4 <- dev4$plot(sendRequest=sendRequest)
    
    devs <- mgr$listCharts()
    
    ids <- c(chart1$getId(), chart2$getId(), chart3$getId(), chart4$getId())
    if (sendRequest) {
      print(mgr$chartList)
      print(mgr$chartList[ids])
      print(ids)
      expect_false(any(sapply(mgr$chartList[ids], is.null)))
    }

    type <- c("blocksTrack", "blocksTrack", "lineTrack", "geneScatterPlot")
    ms <- c(dev1$getId(), dev2$getId(), 
            paste0(dev3$getId(), "$score"),
            paste0(dev4$getId(), "$SAMP_", 1:2, collapse=","))
    connected <- if (sendRequest) rep("*", 4) else rep("", 4)
    expected_df <- data.frame(id=ids,
                              type=type,
                              measurements=ms,
                              connected=connected,
                              stringsAsFactors=FALSE)

    # print(devs); print(expected_df)
    expect_equal(devs, expected_df)
  }, finally=mgr$stopServer())
})

test_that("rmAllCharts works", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  eset <- makeEset()

  mgr <- .startMGR(openBrowser=sendRequest)
  tryCatch({
    dev1 <- mgr$addMeasurements(gr1, "dev1", sendRequest=sendRequest); devId1=dev1$getId()
    dev2 <- mgr$addMeasurements(gr2, "dev2", sendRequest=sendRequest); devId2=dev2$getId()
    dev3 <- mgr$addMeasurements(gr3, "dev3", sendRequest=sendRequest, type="bp"); devId3=dev3$getId()
    dev4 <- mgr$addMeasurements(eset, "dev4", sendRequest = sendRequest, columns=c("SAMP_1", "SAMP_2")); devId4=dev4$getId()
    
    chart1 <- dev1$plot(sendRequest=sendRequest)
    chart2 <- dev2$plot(sendRequest=sendRequest)
    chart3 <- dev3$plot(sendRequest=sendRequest)
    chart4 <- dev4$plot(sendRequest=sendRequest)
    
    devs <- mgr$listCharts()
    
    ids <- c(chart1$getId(), chart2$getId(), chart3$getId(), chart4$getId())
    if (sendRequest) {
      print(mgr$chartList)
      print(mgr$chartList[ids])
      print(ids)
      expect_false(any(sapply(mgr$chartList[ids], is.null)))
    }

    type <- c("blocksTrack", "blocksTrack", "lineTrack", "geneScatterPlot")
    ms <- c(dev1$getId(), dev2$getId(), 
            paste0(dev3$getId(), "$score"),
            paste0(dev4$getId(), "$SAMP_", 1:2, collapse=","))
    connected <- if (sendRequest) rep("*", 4) else rep("", 4)
    expected_df <- data.frame(id=ids,
                              type=type,
                              measurements=ms,
                              connected=connected,
                              stringsAsFactors=FALSE)

    # print(devs); print(expected_df)
    expect_equal(devs, expected_df)
    mgr$rmAllCharts()
    expect_true(length(mgr$chartList) == 0)
  }, finally=mgr$stopServer())
})
