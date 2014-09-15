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

require(RNAseqData.HNRNPC.bam.chr14)
fls <- RNAseqData.HNRNPC.bam.chr14_BAMFILES

require(GenomicFiles)
bfv <- BamFileViews(fls)

ms3 <- mgr$addMeasurements(BamFileViews(fls), "bamv", coverage.only=TRUE)

MAP <- function(RANGE, FILE, ...) {
  browser()
  param <- ScanBamParam(which=RANGE)
  cov <- coverage(path(FILE), param=param)
  cov
}

REDUCE <- function(MAPPED, ...) {
  browser()
  m <- simplify2array(MAPPED)
  m
}


ranges1 <- GRanges("chr14", IRanges(c(19411677, 19659063),width=20))
bfv1 <- BamFileViews(fls, fileRange=ranges1)
res <- reduceByRange(bfv1, MAP, REDUCE)



require(epivizr)
mgr <- startEpiviz(chr="chr2",start=100,end=2000,verbose=TRUE)

library(rtracklayer)

mgr <- startStandalone()
fl <- BigWigFile(system.file("tests", "test.bw", package = "rtracklayer"))
mgr$addSeqinfo(seqinfo(fl))

ms <- mgr$addMeasurements(fl, "cov")
mgr$navigate("chr2", 300, 1000)

query <- GRanges("chr2", IRanges(300,1000))
tmp <- ms$getRows(query, NULL)

dev <- mgr$addDevice(fl, "cov")



