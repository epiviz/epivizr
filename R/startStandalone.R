startStandalone <- function(geneInfo=NULL, geneInfoName="", seqinfo=NULL,
                            chr=NULL, start=NULL, end=NULL, start.args=list(), ...) {
  if (is.null(geneInfo) && is.null(seqinfo)) {
    stop("error starting standalone, one of 'geneInfo' or 'seqinfo' must not be 'NULL'")
  }
  
  start.args$standalone <- TRUE
  mgr <- do.call("startEpiviz",start.args)
  mgr$waitToClearRequests()

  tryCatch({
    if (!is.null(geneInfo)) {
      seqinfo <- seqinfo(geneInfo)
      mgr$addSeqinfo(seqinfo(geneInfo))
    } else if (!is.null(seqinfo)) {
      mgr$addSeqinfo(seqinfo)
    }
  }, error=function(e) {
    epivizrMsg("error adding seqinfo: ", e)
  })
  mgr$waitToClearRequests()
  
  if (missing(chr) || is.null(chr)) {
    chr <- seqnames(seqinfo)[1]
  }
  
  if (missing(start) || missing(end) || is.null(start) || is.null(end)) {
    start <- unname(round(seqlengths(seqinfo)[chr] * .6))
    end <- unname(round(seqlengths(seqinfo)[chr] * .7))
  }
  
  tryCatch({
    mgr$navigate(chr, start, end)
    mgr$waitToClearRequests()
  }, error=function(e) {
    epivizrMsg("error navigating to starting position")
  })
  
  if (!is.null(geneInfo)) {
    tryCatch({
        dev <- mgr$addDevice(geneInfo, geneInfoName, type="geneInfo", ...)
        mgr$waitToClearRequests()
      
    }, error=function(e) {
      epivizrMsg("error adding geneInfo device: ", e)
    })
  }
  mgr
}
