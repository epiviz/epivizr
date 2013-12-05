context("manage measurements")

sendRequest = getOption("epivizrTestSendRequest")

test_that("getMeasurements works", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  eset <- makeEset()

  mgr <- .startMGR(openBrowser=sendRequest)
  tryCatch({
    if (sendRequest) wait_until(substitute(mgr$server$socketConnected))
    msObj1 <- mgr$addMeasurements(gr1, "dev1", sendRequest=sendRequest); msId1=msObj1$getId()
    msObj2 <- mgr$addMeasurements(gr2, "dev2", sendRequest=sendRequest); msId2=msObj2$getId()
    msObj3 <- mgr$addMeasurements(gr3, "dev3", sendRequest=sendRequest, type="bp"); msId3=msObj3$getId()
    msObj4 <- mgr$addMeasurements(eset, "dev4", sendRequest=sendRequest, columns=c("SAMP_1", "SAMP_2")); msId4=msObj4$getId()

    if (sendRequest) wait_until(substitute(!mgr$server$requestWaiting))
    res <- mgr$getMeasurements()

    out <- list(geneMeasurements=structure(list("dev4$SAMP_1","dev4$SAMP_2"), names=paste0(msId4, c("__SAMP_1","__SAMP_2"))),
                bpMeasurements=structure(list("dev3$score"), names=paste0(msId3,"__score")), 
                blockMeasurements=structure(list("dev1","dev2"), names=c(msId1,msId2)))

    expect_equal(res,out)
  }, finally=mgr$stopServer())
})

test_that("rmMeasurements works", {
  sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  
  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    if (sendRequest) wait_until(substitute(mgr$server$socketConnected))
    dev <- mgr$addMeasurements(gr, "dev1", sendRequest=sendRequest, type="bp")
    devId <- dev$getId()
    wait_until(substitute(!mgr$server$requestWaiting))
    mgr$rmMeasurements(dev)
    
    expect_equal(length(mgr$msList$bp), 0)
    expect_true(is.null(mgr$msList$bp[[devId]]))

    dev <- mgr$addMeasurements(gr, "dev1", sendRequest=sendRequest, type="bp")
    devId <- dev$getId()
    if (sendRequest) wait_until(substitute(!mgr$server$requestWaiting))
    mgr$rmMeasurements(devId)
    
    expect_equal(length(mgr$msList$bp), 0)
    expect_true(is.null(mgr$msList$bp[[devId]]))

  },finally=mgr$stopServer())
})

test_that("listMeasurements works", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  eset <- makeEset()

  mgr <- .startMGR(openBrowser=sendRequest)
  tryCatch({
    if (sendRequest) wait_until(substitute(mgr$server$socketConnected))
    dev1 <- mgr$addMeasurements(gr1, "dev1", sendRequest=sendRequest); devId1=dev1$getId()
    dev2 <- mgr$addMeasurements(gr2, "dev2", sendRequest=sendRequest); devId2=dev2$getId()
    dev3 <- mgr$addMeasurements(gr3, "dev3", sendRequest=sendRequest, type="bp"); devId3=dev3$getId()
    dev4 <- mgr$addMeasurements(eset, "dev4", sendRequest = sendRequest, columns=c("SAMP_1", "SAMP_2")); devId4=dev4$getId()

    if (sendRequest) wait_until(substitute(!mgr$server$requestWaiting))
    devs <- mgr$listMeasurements()
    expected_df <- list(gene=data.frame(id=devId4,
                             name="dev4",
                             length=length(dev4$object),
                             connected=ifelse(sendRequest,"*",""),
                             columns=paste0("SAMP_",1:2,collapse=","),
                             stringsAsFactors=FALSE),
                        bp=data.frame(id=devId3,
                                      name="dev3",
                                      length=length(gr3),
                                      connected=ifelse(sendRequest,"*",""),
                                      columns="score",
                                      stringsAsFactors=FALSE),
                        block=data.frame(id=c(devId1,devId2),
                              name=c("dev1","dev2"),
                              length=c(length(gr1),length(gr2)),
                              connected=ifelse(sendRequest,c("*","*"),c("","")),
                              columns=c("",""),
                              stringsAsFactors=FALSE)
                        )
    expect_equal(devs, expected_df)
  }, finally=mgr$stopServer())
})

test_that("rmAllMeasurements works", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  eset <- makeEset()

  mgr <- .startMGR(openBrowser=sendRequest)
  tryCatch({
    if (sendRequest) wait_until(substitute(mgr$server$socketConnected))
    dev1 <- mgr$addMeasurements(gr1, "dev1", sendRequest=sendRequest); devId1=dev1$getId()
    dev2 <- mgr$addMeasurements(gr2, "dev2", sendRequest=sendRequest); devId2=dev2$getId()
    dev3 <- mgr$addMeasurements(gr3, "dev3", sendRequest=sendRequest, type="bp"); devId3=dev3$getId()
    dev4 <- mgr$addMeasurements(eset, "dev4", sendRequest = sendRequest, columns=c("SAMP_1", "SAMP_2")); devId4=dev4$getId()

    if (sendRequest) wait_until(substitute(!mgr$server$requestWaiting))
    devs <- mgr$listMeasurements()
    expected_df <- list(gene=data.frame(id=devId4,
                             name="dev4",
                             length=length(dev4$object),
                             connected=ifelse(sendRequest,"*",""),
                             columns=paste0("SAMP_",1:2,collapse=","),
                             stringsAsFactors=FALSE),
                        bp=data.frame(id=devId3,
                                      name="dev3",
                                      length=length(gr3),
                                      connected=ifelse(sendRequest,"*",""),
                                      columns="score",
                                      stringsAsFactors=FALSE),
                        block=data.frame(id=c(devId1,devId2),
                              name=c("dev1","dev2"),
                              length=c(length(gr1),length(gr2)),
                              connected=ifelse(sendRequest,c("*","*"),c("","")),
                              columns=c("",""),
                              stringsAsFactors=FALSE)
                        )
    expect_equal(devs, expected_df)
    mgr$rmAllMeasurements()
    if (sendRequest) wait_until(substitute(!mgr$server$requestWaiting))
    expect_true(all(sapply(mgr$msList, function(x) length(x))==0))
  }, finally=mgr$stopServer())
})
