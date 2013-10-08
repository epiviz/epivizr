context("update measurement")

test_that("update block works", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  gr2 <- GRanges(seqnames="chr12", ranges=IRanges(start=1:1000,width=10))
  gr3 <- gr1[2:6,]

  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
  	msObj <- mgr$addMeasurements(gr1, "dev1", sendRequest=sendRequest)
	msObj$update(gr2, sendRequest=sendRequest)
	expect_identical(as(msObj$object, "GRanges"), gr2)
	msId <- msObj$getId()
	expect_identical(as(mgr$msList$block[[msId]]$obj$object, "GRanges"), gr2)

	mgr$updateMeasurements(msId, gr3, sendRequest=sendRequest)
	expect_identical(as(msObj$object, "GRanges"), gr3)
	expect_identical(as(mgr$msList$block[[msId]]$obj$object, "GRanges"), gr3)
  }, finally=mgr$stopServer())
})

test_that("update block works with charts", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  gr2 <- GRanges(seqnames="chr12", ranges=IRanges(start=1:1000,width=10))
  gr3 <- gr1[2:6,]

  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
  	msObj <- mgr$addMeasurements(gr1, "dev1", sendRequest=sendRequest)
  	chartObj1 <- msObj$plot(sendRequest=sendRequest)
  	chartObj2 <- msObj$plot(sendRequest=sendRequest)

	msObj$update(gr2, sendRequest=sendRequest)
	expect_identical(as(msObj$object, "GRanges"), gr2)
	msId <- msObj$getId()
	expect_identical(as(mgr$msList$block[[msId]]$obj$object, "GRanges"), gr2)

	mgr$updateMeasurements(msId, gr3, sendRequest=sendRequest)
	expect_identical(as(msObj$object, "GRanges"), gr3)
	expect_identical(as(mgr$msList$block[[msId]]$obj$object, "GRanges"), gr3)
  }, finally=mgr$stopServer())
})

test_that("update block works with device", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  gr2 <- GRanges(seqnames="chr12", ranges=IRanges(start=1:1000,width=10))
  gr3 <- gr1[2:6,]

  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
  	devObj <- mgr$addDevice(gr1, "dev1", sendRequest=sendRequest)  	
	devObj$update(gr2, sendRequest=sendRequest)
	expect_identical(as(devObj$getMsObject()$object, "GRanges"), gr2)
	msId <- devObj$getMsId()
	expect_identical(as(mgr$msList$block[[msId]]$obj$object, "GRanges"), gr2)

	mgr$updateDevice(devObj, gr3, sendRequest=sendRequest)
	expect_identical(as(devObj$getMsObject()$object, "GRanges"), gr3)
	expect_identical(as(mgr$msList$block[[msId]]$obj$object, "GRanges"), gr3)
  }, finally=mgr$stopServer())
})

test_that("update bp works", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  gr2 <- GRanges(seqnames="chr12", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  gr3 <- gr1[2:3,]
  gr4 <- gr1
  gr4$score1 <- NULL

  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
  	msObj <- mgr$addMeasurements(gr1, "dev1", sendRequest=sendRequest, type="bp")
	msObj$update(gr2, sendRequest=sendRequest)
	expect_identical(as(msObj$object, "GRanges"), gr2)
	msId <- msObj$getId()
	expect_identical(as(mgr$msList$bp[[msId]]$obj$object, "GRanges"), gr2)

	mgr$updateMeasurements(msId, gr3, sendRequest=sendRequest)
	expect_identical(as(msObj$object, "GRanges"), gr3)
	expect_identical(as(mgr$msList$bp[[msId]]$obj$object, "GRanges"), gr3)

	expect_error(msObj$update(gr4))
  }, finally=mgr$stopServer())

}) 

test_that("update bp works with charts", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  gr2 <- GRanges(seqnames="chr12", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  gr3 <- gr1[2:3,]
  gr4 <- gr1
  gr4$score1 <- NULL

  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
  	msObj <- mgr$addMeasurements(gr1, "dev1", sendRequest=sendRequest, type="bp")
  	chartObj1 <- msObj$plot(sendRequest=sendRequest)
  	chartObj2 <- msObj$plot(sendRequest=sendRequest)

	msObj$update(gr2, sendRequest=sendRequest)
	expect_identical(as(msObj$object, "GRanges"), gr2)
	msId <- msObj$getId()
	expect_identical(as(mgr$msList$bp[[msId]]$obj$object, "GRanges"), gr2)

	mgr$updateMeasurements(msId, gr3, sendRequest=sendRequest)
	expect_identical(as(msObj$object, "GRanges"), gr3)
	expect_identical(as(mgr$msList$bp[[msId]]$obj$object, "GRanges"), gr3)

	expect_error(msObj$update(gr4))
  }, finally=mgr$stopServer())

}) 

test_that("update bp works with device", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  gr2 <- GRanges(seqnames="chr12", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  gr3 <- gr1[2:3,]
  gr4 <- gr1
  gr4$score1 <- NULL

  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
  	devObj <- mgr$addDevice(gr1, "dev1", sendRequest=sendRequest, type="bp")

	devObj$update(gr2, sendRequest=sendRequest)
	expect_identical(as(devObj$getMsObject()$object, "GRanges"), gr2)
	msId <- devObj$getMsId()
	expect_identical(as(mgr$msList$bp[[msId]]$obj$object, "GRanges"), gr2)

	mgr$updateDevice(devObj$getId(), gr3, sendRequest=sendRequest)
	expect_identical(as(devObj$getMsObject()$object, "GRanges"), gr3)
	expect_identical(as(mgr$msList$bp[[msId]]$obj$object, "GRanges"), gr3)

	expect_error(devObj$update(gr4))
  }, finally=mgr$stopServer())

}) 

test_that("update feature works", {
  sendRequest=sendRequest
  sset <- makeSExp()
  sset2 <- sset[2:10,]
  sset3 <- sset2[,-1]

  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    msObj <- mgr$addMeasurements(sset, "ms1", sendRequest=sendRequest, columns=c("A","B"), assay="counts2")
	msObj$update(sset2, sendRequest=sendRequest)
	expect_identical(as(rowData(msObj$object), "GRanges"), rowData(sset2))
	expect_identical(assays(msObj$object), assays(sset2))
	expect_identical(colData(msObj$object), colData(sset2))

	msId <- msObj$getId()
	tmp <- mgr$msList$gene[[msId]]$obj$object
	expect_identical(as(rowData(tmp), "GRanges"), rowData(sset2))
	expect_identical(colData(tmp), colData(sset2))
	expect_identical(assays(tmp), assays(sset2))

	expect_error(mgr$updateMeasurements(msId, gr3, sendRequest=sendRequest))
  }, finally=mgr$stopServer())

}) 

test_that("update feature works", {
  sendRequest=sendRequest
  sset <- makeSExp()
  sset2 <- sset[2:10,]
  sset3 <- sset2[,-1]

  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    msObj <- mgr$addMeasurements(sset, "ms1", sendRequest=sendRequest, columns=c("A","B"), assay="counts2")
	msObj$update(sset2, sendRequest=sendRequest)
	expect_identical(as(rowData(msObj$object), "GRanges"), rowData(sset2))
	expect_identical(assays(msObj$object), assays(sset2))
	expect_identical(colData(msObj$object), colData(sset2))

	msId <- msObj$getId()
	tmp <- mgr$msList$gene[[msId]]$obj$object
	expect_identical(as(rowData(tmp), "GRanges"), rowData(sset2))
	expect_identical(colData(tmp), colData(sset2))
	expect_identical(assays(tmp), assays(sset2))

	expect_error(mgr$updateMeasurements(msId, gr3, sendRequest=sendRequest))
  }, finally=mgr$stopServer())

}) 

test_that("update feature works with charts", {
  sendRequest=sendRequest
  sset <- makeSExp()
  sset2 <- sset[2:10,]
  sset3 <- sset2[,-1]

  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    msObj <- mgr$addMeasurements(sset, "ms1", sendRequest=sendRequest, columns=c("A","B"), assay="counts2")
    chartObj1 <- msObj$plot(sendRequest=sendRequest)
    chartObj2 <- msObj$plot(sendRequest=sendRequest)

	msObj$update(sset2, sendRequest=sendRequest)
	expect_identical(as(rowData(msObj$object), "GRanges"), rowData(sset2))
	expect_identical(assays(msObj$object), assays(sset2))
	expect_identical(colData(msObj$object), colData(sset2))

	msId <- msObj$getId()
	tmp <- mgr$msList$gene[[msId]]$obj$object
	expect_identical(as(rowData(tmp), "GRanges"), rowData(sset2))
	expect_identical(colData(tmp), colData(sset2))
	expect_identical(assays(tmp), assays(sset2))

	expect_error(mgr$updateMeasurements(msId, gr3, sendRequest=sendRequest))
  }, finally=mgr$stopServer())

}) 

test_that("update feature works with device", {
  sendRequest=sendRequest
  sset <- makeSExp()
  sset2 <- sset[2:10,]
  sset3 <- sset2[,-1]

  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    devObj <- mgr$addDevice(sset, "ms1", sendRequest=sendRequest, columns=c("A","B"), assay="counts2")

	devObj$update(sset2, sendRequest=sendRequest)
	expect_identical(as(rowData(devObj$getMsObject()$object), "GRanges"), rowData(sset2))
	expect_identical(assays(devObj$getMsObject()$object), assays(sset2))
	expect_identical(colData(devObj$getMsObject()$object), colData(sset2))

	msId <- devObj$getMsId()
	tmp <- mgr$msList$gene[[msId]]$obj$object
	expect_identical(as(rowData(tmp), "GRanges"), rowData(sset2))
	expect_identical(colData(tmp), colData(sset2))
	expect_identical(assays(tmp), assays(sset2))

	expect_error(mgr$updateDevice(devObj, gr3, sendRequest=sendRequest))
  }, finally=mgr$stopServer())

}) 
