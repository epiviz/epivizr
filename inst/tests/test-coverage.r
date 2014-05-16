require(ShortRead)

 fl <- system.file("extdata", "ex1.bam", package="Rsamtools", mustWork=TRUE)
gal <- readGAlignments(fl)
gal <- renameSeqlevels(gal, c(seq1="chr1", seq2="chr2"))

require(epivizr)
mgr <- startEpiviz(chr="chr1",start=100,end=1000,verbose=TRUE)
ms <- mgr$addMeasurements(gal, "cov", coverage.only=TRUE)
ch <- ms$plot()
dev <- mgr$addDevice(gal, "cov", coverage.only=TRUE)

ms2 <- mgr$addMeasurements(BamFile(fl), "bam", coverage.only=TRUE)
ch2 <- ms2$plot()
dev2 <- mgr$addDevice(BamFile(fl), "bam", coverage.only=TRUE)
