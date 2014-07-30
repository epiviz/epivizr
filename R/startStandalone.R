startStandalone <- function(geneInfo=NULL, geneInfoName="",
                            chr="", start=1, end=1, ...) {
  mgr <- startEpiviz(standalone=TRUE, ...)
  mgr$waitToClearRequests()
  mgr$addSeqinfo(seqinfo(geneInfo))
  mgr$waitToClearRequests()
  dev <- mgr$addDevice(geneInfo, geneInfoName, type="geneInfo")
  mgr$waitToClearRequests()
  obj <- dev$getMsObject()$object
  
  if (any(c(missing(chr), missing(start), missing(end)))) {
    rng <- ranges(obj)[1]
    rng <- resize(rng, width=1.5*width(rng), fix="center")
    mgr$navigate(chr=as.character(seqnames(obj)[1]),
                 start=start(rng), end=end(rng))
  } else {
    mgr$navigate(chr=chr, start=start, end=end)
  }
  mgr$waitToClearRequests()
  mgr
}
