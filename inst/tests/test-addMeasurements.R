context("addMeasurements")

sendRequest=getOption("epivizrTestSendRequest")

test_that("addMeasurements works for blocks", {
  sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(substitute(mgr$server$socketConnected))
    msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest)
    msId <- msObj$getId()
    
    expect_equal(length(mgr$msList$block), 1)
    expect_false(is.null(mgr$msList$block[[msId]]))
    expect_equal(mgr$msList$block[[msId]]$name, "ms1")
    expect_equal(mgr$msList$block[[msId]]$measurements, msId)

    expect_equal(as(mgr$msList$block[[msId]]$obj$object, "GRanges"), unname(gr))

    if (sendRequest) wait_until(substitute(!mgr$server$requestWaiting))
    connected <- mgr$msList$block[[msId]]$connected
    expect_equal(connected, sendRequest)

  }, finally=mgr$stopServer())
})

test_that("addMeasurements works for bp", {
  sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    if (sendRequest) wait_until(substitute(mgr$server$socketConnected))
    msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest, type="bp")
    msId <- msObj$getId()
    
    expect_equal(length(mgr$msList$bp), 1)
    expect_false(is.null(mgr$msList$bp[[msId]]))
    expect_equal(mgr$msList$bp[[msId]]$name, "ms1")
    expect_equal(mgr$msList$bp[[msId]]$measurements, paste0(msId,"__","score",1:2))
    expect_equal(as(mgr$msList$bp[[msId]]$obj$object, "GRanges"), unname(gr))
    expect_equal(mgr$msList$bp[[msId]]$obj$columns, paste0("score",1:2))

    if (sendRequest) wait_until(substitute(!mgr$server$requestWaiting))
    connected <- mgr$msList$bp[[msId]]$connected
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("addMeasurements works for SummarizedExperiment", {
  sendRequest=sendRequest
  sset <- makeSExp()
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(substitute(mgr$server$socketConnected))
    msObj <- mgr$addMeasurements(sset, "ms1", sendRequest=sendRequest, columns=c("A","B"), assay="counts2")
    msId <- msObj$getId()

    expect_equal(length(mgr$msList$gene), 1)
    expect_false(is.null(mgr$msList$gene[[msId]]))
    expect_equal(mgr$msList$gene[[msId]]$name, "ms1")
    expect_equal(mgr$msList$gene[[msId]]$measurements, paste0(msId, "__", c("A","B")))
    expect_equal(mgr$msList$gene[[msId]]$obj$columns, c("A","B"))

    if (sendRequest) wait_until(substitute(!mgr$server$requestWaiting))
    connected <- mgr$msList$gene[[msId]]$connected
    expect_equal(connected, sendRequest)    
  }, finally=mgr$stopServer())
})

test_that("addMeasurements works for ExpressionSet", {
  sendRequest=sendRequest
  eset <- makeEset()
  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    if (sendRequest) wait_until(substitute(mgr$server$socketConnected))
    msObj <- mgr$addMeasurements(eset, "ms1", sendRequest=sendRequest, columns=c("SAMP_1","SAMP_2"))
    msId <- msObj$getId()
    
    expect_equal(length(mgr$msList$gene), 1)
    expect_false(is.null(mgr$msList$gene[[msId]]))
    expect_equal(mgr$msList$gene[[msId]]$name, "ms1")
    expect_equal(mgr$msList$gene[[msId]]$measurements, paste0(msId,"__","SAMP_",1:2))
    expect_equal(mgr$msList$gene[[msId]]$obj$columns, paste0("SAMP_",1:2))

    if (sendRequest) wait_until(substitute(!mgr$server$requestWaiting))
    connected <- mgr$msList$gene[[msId]]$connected
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

