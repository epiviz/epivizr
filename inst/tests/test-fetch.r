context("data fetch")

sendRequest=getOption("epivizrTestSendRequest")

test_that("block data fetch works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1),
                 seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
  msObj1 <- epivizr::register(gr1)
  expect_is(msObj1, "EpivizBlockData")
  query <- GRanges(seqnames="chr1", ranges=IRanges(start=2, end=6))
  res <- msObj1$getRows(query,character())
  out <- list(globalStartIndex=2,
              useOffset=FALSE,
              values=list(id=2:6,
                start=2:6,
                end=2:6,
                metadata=NULL))
  expect_equal(res,out)
})

test_that("msmt fetch works on unsorted data", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=10:1, width=1),
                 seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
  msObj1 <- epivizr::register(gr1)

  query <- GRanges(seqnames="chr1", ranges=IRanges(start=2, end=6))
  res <- msObj1$getRows(query,NULL)
  out <- list(globalStartIndex=2,
              useOffset=FALSE,
              values=list(id=2:6,
                start=2:6,
                end=2:6,
                metadata=(NULL)))
  expect_equal(res,out)
})

test_that("device data fetch works on bp data", {
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=5), width=1), score1=seq(1,100,by=5), score2=-seq(1,100,by=5),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  
  msObj1 <- epivizr::register(gr3, type="bp")
  expect_is(msObj1, "EpivizBpData")

  query <- GRanges(seqnames="chr1", ranges=IRanges(start=2,end=6))
  res <- msObj1$getRows(query, NULL)
  out <- list(globalStartIndex=2,
              useOffset=FALSE,
              values=list(id=list(2),
                start=list(6),
                end=list(6),
                metadata=NULL))

  expect_equal(res, out)
  #print(res);print(out)
  res <- msObj1$getValues(query, c("score1"))
  out <- list(globalStartIndex=2,
              values=list(6))
  expect_equal(res,out)  
})

#test_that("device data fetch works on bp data with NAs", {
#  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=5), width=1), score1=seq(1,100,by=5), score2=-seq(1,100,by=5),
#                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
#  gr3$score2[1:10]=NA
#  
#  msObj1 <- epivizr::register(gr3, type="bp")
#  expect_is(msObj1, "EpivizBpData")
#  query <- GRanges("chr1", IRanges(start=2, end=6))
#  dataPack <- EpivizBpData$new()$.initPack(2L)
#  dataPack$set(msObj1$getData(query=query,
#                              msId="bp1$score1"),
#               msId="bp1$score1",
#               index=1)
#  dataPack$set(msObj1$getData(query=query,
#                              msId="bp1$score2"),
#               msId="bp1$score2",
#               index=2)
#  res <- dataPack$getData()
#
#  out=list()
#  lims <- cbind(range(pretty(seq(1,96,len=10))),
#                range(pretty(seq(-96,-51,len=10))))
#  out$min=structure(lims[1,], names=paste0("bp1$score", 1:2))
#  out$max=structure(lims[2,], names=paste0("bp1$score", 1:2))
#  out$data=structure(list(list(bp=6,value=6),list(bp=integer(),value=numeric())), names=paste0("bp1$score", 1:2))
#
#   # cat("res\n"); print(res)
#   # cat("out\n"); print(out)
#
# expect_equal(res,out)
#})
#

test_that("feature data fetch works", {
  eset <- makeEset()
  msObj <- epivizr::register(eset, columns=c("SAMP_1", "SAMP_2"))
  query <- GRanges(seqnames="chr6", ranges=IRanges(start=30000000,end=40000000))

  olaps <- findOverlaps(query, msObj$object)
  hits <- unique(subjectHits(olaps))
  hits <- seq(min(hits),max(hits))
  tmp <- msObj$object[hits,]

  m <- match(rowRanges(tmp)$PROBEID, featureNames(eset))
  mat <- exprs(eset)[m, c("SAMP_1", "SAMP_2")]

  res <- msObj$getRows(query, c("PROBEID","SYMBOL"))
  
  out <- list(globalStartIndex=min(hits),
              useOffset=FALSE,
              values=list(
                id=hits,
                start=start(tmp),
                end=end(tmp),
                metadata=list(PROBEID=rowRanges(tmp)$PROBEID,
                  SYMBOL=rowRanges(tmp)$SYMBOL)
                ))
  expect_equal(res, out)
  #print(res); print(out)
  
  res <- msObj$getValues(query, "SAMP_1")
  out <- list(globalStartIndex=min(hits),
              values=unname(mat[,"SAMP_1"]))
  #print(res);print(out)
  expect_equal(res,out)
})

test_that("geneinfo fetch works", {
  sendRequest <- sendRequest
  gr <- makeGeneInfo()
  msmt <- epivizr::register(gr, type="geneInfo")
  query <- GRanges("chr11", IRanges(start=102500000, end=103000000))
  res <- msmt$getRows(query, c("gene", "exon_starts", "exon_ends"))
  
  msGR <- msmt$object
  olaps <- findOverlaps(query, msGR)
  hits <- subjectHits(olaps)
  hits <- seq(min(hits), max(hits))
  tmp <- msGR[hits,]
  
  out <- list(globalStartIndex=hits[1],
              useOffset=FALSE,
              values=list(
                id=hits,
                start=start(tmp),
                end=end(tmp),
                metadata=list(gene=unname(as.character(tmp$Gene)),
                              exon_starts=unname(lapply(start(tmp$Exons),paste,collapse=",")),
                              exon_ends=unname(lapply(end(tmp$Exons), paste, collapse=","))),
                strand=unname(as.character(strand(tmp)))))
  #print(res); print(out)
  expect_equal(res, out)
})

test_that("mgr fetch works", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr6", ranges=IRanges(start=30000000+(1:10), width=100),
                 seqinfo=Seqinfo(seqnames=c("chr6","chr7"),genome="hcb"))
  gr2 <- GRanges(seqnames="chr7", ranges=IRanges(start=30000000+(2:20), width=100),
                 seqinfo=Seqinfo(seqnames=c("chr6","chr7"),genome="hcb"))
  gr3 <- GRanges(seqnames="chr6", ranges=IRanges(start=30000000+seq(1,100,by=5), width=1), score1=seq(1,100,by=5), score2=-seq(1,100,by=5),
                 seqinfo=Seqinfo(seqnames=c("chr6","chr7"),genome="hcb"))
  eset <- makeEset()


  tryCatch({
    mgr <- .startMGR(openBrowser=sendRequest, chr="chr6", start=30000000, end=40000000)
  
    dev1 <- mgr$addMeasurements(gr1, "dev1",sendRequest=sendRequest); devId1=dev1$getId()
    dev2 <- mgr$addMeasurements(gr2, "dev2",sendRequest=sendRequest); devId2=dev2$getId()
    dev3 <- mgr$addMeasurements(gr3, "dev3", sendRequest=sendRequest, type="bp"); devId3=dev3$getId()
    dev4 <- mgr$addMeasurements(eset, "dev4", sendRequest=sendRequest, columns=c("SAMP_1", "SAMP_2")); devId4=dev4$getId()

    m <- match(rowRanges(dev4$object)$PROBEID, featureNames(eset))
    mat <- exprs(eset)[m,c("SAMP_1","SAMP_2")]
    lims <- unname(apply(mat, 2, function(x) range(pretty(range(x)))))

    query <- GRanges(seqnames="chr6",ranges=IRanges(start=30000000,end=40000000))

    hits <- unique(subjectHits(findOverlaps(query, dev4$object)))
    hits <- seq(min(hits), max(hits))
    tmp <- dev4$object[hits,]
    
    m <- match(rowRanges(tmp)$PROBEID, featureNames(eset))
    mat <- exprs(eset)[m,c("SAMP_1","SAMP_2")]
    
    if (sendRequest) { 
      tryCatch(mgr$service(),interrupt=function(e) NULL)
    }

    # dev 1
    res <- mgr$getRows(seqnames(query),start(query),end(query),NULL,devId1)
    out <- list(globalStartIndex=1,
                useOffset=FALSE,
                values=list(
                  id=1:10,
                  start=30000000+(1:10),
                  end=30000000+(100:109),
                  metadata=NULL))
    expect_equal(res,out)

    # dev 2
    res <- mgr$getRows(seqnames(query),start(query),end(query),NULL,devId2)
    out <- list(globalStartIndex=NULL,
                useOffset=FALSE,values=list(
                                  id=list(),
                                  start=list(),
                                  end=list(),
                                  metadata=NULL))
    expect_equal(res,out)

    # dev 3
    res <- mgr$getRows(seqnames(query),start(query),end(query),NULL,devId3)
    out <- list(globalStartIndex=1,
                useOffset=FALSE,
                values=list(
                  id=seq(len=length(seq(1,100,by=5))),
                  start=30000000+seq(1,100,by=5),
                  end=30000000+seq(1,100,by=5),
                  metadata=NULL))
    expect_equal(res,out)

    res <- mgr$getValues(seqnames(query),start(query),end(query),devId3,"score1")
    out <- list(globalStartIndex=1,
                values=seq(1,100,by=5))
    expect_equal(res,out)

    res <- mgr$getValues(seqnames(query),start(query),end(query),devId3,"score2")
    out <- list(globalStartIndex=1,
                values=-seq(1,100,by=5))
    expect_equal(res,out)

    # dev 4
    res <- mgr$getRows(seqnames(query),start(query),end(query),c("PROBEID","SYMBOL"),devId4)
    out <- list(globalStartIndex=hits[1],
                useOffset=FALSE,
                values=list(id=hits,
                  start=start(tmp),
                  end=end(tmp),
                  metadata=list(PROBEID=rowRanges(tmp)$PROBEID,
                    SYMBOL=rowRanges(tmp)$SYMBOL)))
    expect_equal(res,out)

    res <- mgr$getValues(seqnames(query),start(query),end(query),devId4,"SAMP_1")
    out <- list(globalStartIndex=hits[1],
                values=unname(mat[,"SAMP_1"]))
    expect_equal(res,out)

    res <- mgr$getValues(seqnames(query),start(query),end(query),devId4,"SAMP_2")
    out <- list(globalStartIndex=hits[1],
                values=unname(mat[,"SAMP_2"]))
    expect_equal(res,out)
    
  }, finally=mgr$stopServer())
})

#test_that("mgr fetch with charts", {
#  sendRequest=sendRequest
#  gr1 <- GRanges(seqnames="chr6", ranges=IRanges(start=30000000+(1:10), width=100),
#                 seqinfo=Seqinfo(seqnames=c("chr6","chr7"),genome="hcb"))
#  gr2 <- GRanges(seqnames="chr7", ranges=IRanges(start=30000000+(2:20), width=100),
#                 seqinfo=Seqinfo(seqnames=c("chr6","chr7"),genome="hcb"))
#  gr3 <- GRanges(seqnames="chr6", ranges=IRanges(start=30000000+seq(1,100,by=5), width=1), score1=seq(1,100,by=5), score2=-seq(1,100,by=5),
#                 seqinfo=Seqinfo(seqnames=c("chr6","chr7"),genome="hcb"))
#  eset <- makeEset()
#
#
#  tryCatch({
#    mgr <- .startMGR(openBrowser=sendRequest, chr="chr6", start=30000000, end=40000000)
#  
#    dev1 <- mgr$addDevice(gr1, "dev1",sendRequest=sendRequest); devId1=dev1$getMsId()
#    dev2 <- mgr$addDevice(gr2, "dev2",sendRequest=sendRequest); devId2=dev2$getMsId()
#    dev3 <- mgr$addDevice(gr3, "dev3", sendRequest=sendRequest, type="bp"); devId3=dev3$getMsId()
#    dev4 <- mgr$addDevice(eset, "dev4", sendRequest=sendRequest, columns=c("SAMP_1", "SAMP_2")); devId4=dev4$getMsId()
#
#    m <- match(rowData(dev4$getMsObject()$object)$PROBEID, featureNames(eset))
#    mat <- exprs(eset)[m,c("SAMP_1","SAMP_2")]
#    lims <- unname(apply(mat, 2, function(x) range(pretty(range(x)))))
#
#    query <- GRanges(seqnames="chr6",ranges=IRanges(start=30000000,end=40000000))
#
#    tmp <- subsetByOverlaps(dev4$getMsObject()$object, query)
#    o <- order(start(tmp))
#    m <- match(rowData(tmp)$PROBEID[o], featureNames(eset))
#    mat <- exprs(eset)[m,c("SAMP_1","SAMP_2")]
#    
#    if (sendRequest) { 
#      tryCatch(mgr$service(),interrupt=function(e) NULL)
#    }
#    
#    measurements=list(geneMeasurements=paste0(devId4,"$SAMP_", 1:2), 
#                      bpMeasurements=paste0(devId3,"$score",1:2),
#                      blockMeasurements=c(devId1,devId2))
#
#    res <- mgr$getData(measurements, chr="chr6", start=30000000, end=40000000)
#    
#    out <- list(chr="chr6",start=30000000,end=40000000)
#    out$geneData=list(start=30000000,end=40000000,chr="chr6")
#    out$geneData$min=structure(lims[1,],names=paste0(devId4,"$","SAMP_",1:2))
#    out$geneData$max=structure(lims[2,],names=paste0(devId4,"$","SAMP_",1:2))
#    out$geneData$data=list(gene=rowData(tmp)$SYMBOL[o],
#                   start=start(tmp)[o],
#                   end=end(tmp)[o],
#                   probe=rowData(tmp)$PROBEID[o],
#                   unname(mat[,1]),
#                   unname(mat[,2]))
#    names(out$geneData$data)[5:6]=paste0(devId4,"$SAMP_",1:2)
#
#    out$bpData=list(start=30000000,end=40000000,chr="chr6")
#    out$bpData$min=structure(c(0,-100),names=paste0(devId3,"$","score",1:2))
#    out$bpData$max=structure(c(100,0),names=paste0(devId3,"$","score",1:2))
#    out$bpData$data=structure(list(list(bp=30000000+seq(1,100,by=5),value=seq(1,100,by=5)),
#                                   list(bp=30000000+seq(1,100,by=5),value=-seq(1,100,by=5))),
#                              names=paste0(devId3,"$","score",1:2))
#    
#    out$blockData=list(start=30000000,end=40000000,chr="chr6")
#    out$blockData$data=structure(list(list(start=30000000+(1:10), end=30000000+(100:109)),
#                                      list(start=integer(), end=integer())),
#                                 names=c(devId1,devId2))
#    
#    # cat("res\n"); print(res$blockData)
#    # cat("out\n"); print(out$blockData)
#
#    expect_equal(res$geneData,out$geneData)
#    expect_equal(res$bpData,out$bpData)
#    expect_equal(res$blockData,out$blockData)
#  }, finally=mgr$stopServer())
#})
#
# test_that("mgr fetch no data works", {
#   gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100),
#                  seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
#   gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100),
#                  seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
#   gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=5), width=1), score=seq(1,100,by=5),
#                  seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  
#   tryCatch({
#     sendRequest=sendRequest
#     mgr <- .startMGR(openBrowser=sendRequest)
    
#     dev1 <- mgr$addDevice(gr1, "dev1"); devId1=dev1$id
#     dev2 <- mgr$addDevice(gr2, "dev2"); devId2=dev2$id
#     dev3 <- mgr$addDevice(gr3, "dev3", type="bp"); devId3=dev3$id
    
#     if (sendRequest) {
#       tryCatch(mgr$service(), interrupt=function(e) NULL)
#     }
#     measurements=list(bpMeasurements=paste0(devId3,"$score"),blockMeasurements=c(devId1,devId2))
#     res <- mgr$getData(measurements, chr="chr11", start=2, end=6)
    
#     out <- list(chr="chr11",start=2,end=6)
#     out$bpData=list(start=2,end=6,chr="chr11")
#     lim <- range(gr3$score)
#     lim <- range(pretty(range(gr3$score)))
#     out$bpData$min=structure(lim[1],names=paste0(devId3,"$","score"))
#     out$bpData$max=structure(lim[2],names=paste0(devId3,"$","score"))
#     out$bpData$data=structure(list(list(bp=integer(),value=numeric())),names=paste0(devId3,"$","score"))
    
#     out$blockData=list(start=2,end=6,chr="chr11")
#     out$blockData$data=structure(list(list(start=integer(), end=integer()),
#                                       list(start=integer(), end=integer())),
#                                  names=c(devId1,devId2))

#   # cat("res\n"); print(res$bpData)
#   # cat("out\n"); print(out$bpData)

#   expect_equal(res,out)
#   }, finally=mgr$stopServer())
# })

# test_that("data with NAs are handled", {
#     sendRequest=sendRequest
#     gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100),
#                    seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
#     gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100),
#                    seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
#     gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=5), width=1), score1=seq(1,100,by=5), score2=-seq(1,100,by=5),
#                    seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
    
#     gr3$score2[1:10] <- NA
    
#     tryCatch({
#       mgr <- .startMGR(openBrowser=sendRequest, chr="chr1", start=2, end=6)
      
#       dev1 <- mgr$addDevice(gr1, "dev1",sendRequest=sendRequest); devId1=dev1$id
#       dev2 <- mgr$addDevice(gr2, "dev2",sendRequest=sendRequest); devId2=dev2$id
#       dev3 <- mgr$addDevice(gr3, "dev3", sendRequest=sendRequest, type="bp"); devId3=dev3$id
      
#       if (sendRequest) { 
#         tryCatch(mgr$service(),interrupt=function(e) NULL)
#       }
      
#       measurements=list(bpMeasurements=paste0(devId3,"$score",1:2),blockMeasurements=c(devId1,devId2))
#       res <- mgr$getData(measurements, chr="chr1", start=2, end=6)
      
#       out <- list(chr="chr1",start=2,end=6)
#       out$bpData=list(start=2,end=6,chr="chr1")
#       lims1 <- range(pretty(range(gr3$score1,na.rm=TRUE)))
#       lims2 <- range(pretty(range(gr3$score2,na.rm=TRUE)))

#       out$bpData$min=structure(c(lims1[1],lims2[1]),names=paste0(devId3,"$","score",1:2))
#       out$bpData$max=structure(c(lims1[2],lims2[2]),names=paste0(devId3,"$","score",1:2))
#       out$bpData$data=structure(list(list(bp=6,value=6),list(bp=integer(),value=numeric())),names=paste0(devId3,"$","score",1:2))
      
#       out$blockData=list(start=2,end=6,chr="chr1")
#       out$blockData$data=structure(list(list(start=1:6, end=100:105),
#                                         list(start=integer(), end=integer())),
#                                    names=c(devId1,devId2))

      
#       # cat("res\n"); print(res$bpData);
#       # cat("out\n"); print(out$bpData)
#       expect_equal(res,out)
#     }, finally=mgr$stopServer())
# })
