context("addMeasurements")

sendRequest=getOption("epivizrTestSendRequest")

test_that("addMeasurements works for blocks", {
  sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
    msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest)
    msId <- msObj$getId()
    expMs <- list(list(id=msId,
                       name=msObj$name,
                       type="range",
                       datasourceId=msId,
                       datasourceGroup=msId,
                       defaultChartType="Blocks Track",
                       annotation=NULL,
                       minValue=NA,
                       maxValue=NA,
                       metadata=NULL))
    
    expect_equal(length(mgr$msList$block), 1)
    expect_false(is.null(mgr$msList$block[[msId]]))
    expect_equal(mgr$msList$block[[msId]]$name, "ms1")
    expect_equal(mgr$msList$block[[msId]]$measurements, expMs)

    expect_equal(as(mgr$msList$block[[msId]]$obj$object, "GRanges"), unname(gr))

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
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
    if (sendRequest) wait_until(mgr$server$socketConnected)
    msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest, type="bp")
    msId <- msObj$getId()

    rngs <- sapply(1:2, function(i) range(pretty(range(mcols(gr)[,paste0("score",i)], na.rm=TRUE))))
    
    expMs <- lapply(1:2, function(i) {
      list(id=paste0("score",i),
           name=paste0("score",i),
           type="feature",
           datasourceId=msId,
           datasourceGroup=msId,
           defaultChartType="Line Track",
           annotation=NULL,
           minValue=rngs[1,i],
           maxValue=rngs[2,i],
           metadata=NULL)
    })

    obsMs <- mgr$msList$bp[[msId]]$measurements
    
    expect_equal(length(mgr$msList$bp), 1)
    expect_false(is.null(mgr$msList$bp[[msId]]))
    expect_equal(mgr$msList$bp[[msId]]$name, "ms1")
    expect_equal(mgr$msList$bp[[msId]]$measurements, expMs)
    expect_equal(as(mgr$msList$bp[[msId]]$obj$object, "GRanges"), unname(gr))
    expect_equal(mgr$msList$bp[[msId]]$obj$columns, paste0("score",1:2))

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    connected <- mgr$msList$bp[[msId]]$connected
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("addMeasurements works for SummarizedExperiment", {
  sendRequest=sendRequest
  sset <- makeSExp()
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
    msObj <- mgr$addMeasurements(sset, "ms1", sendRequest=sendRequest, columns=c("A","B"), assay="counts2")
    msId <- msObj$getId()

    rngs <- unname(sapply(c("A","B"), function(col) range(pretty(range(assay(sset,"counts2")[,col], na.rm=TRUE)))))
    
    expMs <- lapply(c("A","B"), function(col) {
      i <- match(col,c("A","B"))
      list(id=col,
           name=col,
           type="feature",
           datasourceId=msId,
           datasourceGroup=msId,
           defaultChartType="Scatter Plot",
           annotation=NULL,
           minValue=rngs[1,i],
           maxValue=rngs[2,i],
           metadata=c("probe","symbol"))
    })

    expect_equal(length(mgr$msList$gene), 1)
    expect_false(is.null(mgr$msList$gene[[msId]]))
    expect_equal(mgr$msList$gene[[msId]]$name, "ms1")
    expect_equal(mgr$msList$gene[[msId]]$measurements, expMs)
    expect_equal(mgr$msList$gene[[msId]]$obj$columns, c("A","B"))

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    connected <- mgr$msList$gene[[msId]]$connected
    expect_equal(connected, sendRequest)    
  }, finally=mgr$stopServer())
})

test_that("addMeasurements works for ExpressionSet", {
  sendRequest=sendRequest
  eset <- makeEset()
  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
    msObj <- mgr$addMeasurements(eset, "ms1", sendRequest=sendRequest, columns=c("SAMP_1","SAMP_2"))
    msId <- msObj$getId()

    rngs <- sapply(1:2, function(i) range(pretty(range(exprs(eset)[,paste0("SAMP_",i)]))))
    
    expMS <- lapply(1:2, function(i) {
      list(id=paste0("SAMP_",i),
           name=paste0("SAMP_",i),
           type="feature",
           datasourceId=msId,
           datasourceGroup=msId,
           defaultChartType="Scatter Plot",
           annotation=NULL,
           minValue=rngs[1,i],
           maxValue=rngs[2,i],
           metadata=c("probe","symbol"))
    })

    obsMs <- mgr$msList$gene[[msId]]$measurements
    
    expect_equal(length(mgr$msList$gene), 1)
    expect_false(is.null(mgr$msList$gene[[msId]]))
    expect_equal(mgr$msList$gene[[msId]]$name, "ms1")
    expect_equal(mgr$msList$gene[[msId]]$measurements, expMS)
    expect_equal(mgr$msList$gene[[msId]]$obj$columns, paste0("SAMP_",1:2))

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    connected <- mgr$msList$gene[[msId]]$connected
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

