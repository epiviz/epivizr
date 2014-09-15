library(epivizr)
library(rtracklayer)

mgr <- startStandalone()
fl <- BigWigFile(system.file("tests", "test.bw", package = "rtracklayer"))
mgr$addSeqinfo(seqinfo(fl))

ms <- mgr$addMeasurements(fl, "cov")
mgr$navigate("chr2", 300, 1000)

query <- GRanges("chr2", IRanges(300,1000))
tmp <- ms$getRows(query, NULL)

dev <- mgr$addDevice(fl, "cov")



