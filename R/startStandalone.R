startStandalone <- function(geneInfo=NULL, geneInfoName="", seqinfo=NULL,
                            chr="", start=1, end=1, start.args=list(), ...) {
  start.args$standalone <- TRUE
  mgr <- do.call("startEpiviz",start.args)
  mgr$waitToClearRequests()

  tryCatch({
    if (!is.null(geneInfo)) {
      mgr$addSeqinfo(seqinfo(geneInfo))
    } else if (!is.null(seqinfo)) {
      mgr$addSeqinfo(seqinfo)
    }
  }, error=function(e) {
    epivizrMsg("error adding seqinfo: ", e)
  })
  mgr$waitToClearRequests()

  tryCatch({
    if (!is.null(geneInfo)) {
      dev <- mgr$addDevice(geneInfo, geneInfoName, type="geneInfo", ...)
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
    }
  }, error=function(e) {
    epivizrMsg("error adding geneInfo device: ", e)
  })
  
  mgr
}
