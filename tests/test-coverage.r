library(epivizr)
library(rtracklayer)

fl <- BigWigFile(system.file("tests", "test.bw", package = "rtracklayer"))

mgr <- startStandalone(start.args=list(verbose=TRUE))
mgr$addSeqinfo(seqinfo(fl))

#ms <- mgr$addMeasurements(fl, "cov")
mgr$navigate("chr2", 300, 1000)

query <- GRanges("chr2", IRanges(300,1000))
tmp <- ms$getRows(query, NULL)

dev <- mgr$addDevice(fl, "cov", windowSize=100L)
mgr$stopServer()



