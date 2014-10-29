makeGeneTrackAnnotation <- function(object, kind=c("gene","tx"), keepSeqlevels=NULL)
{
  kind <- match.arg(kind)
  gr <- genes(object, columns=c("GENEID", "SYMBOL"))
  exons <- exonsBy(object, by=kind)
  
  ids <- as.character(gr$GENEID)
  exons <- reduce(ranges(exons)[ids])
  gr$Exons <- exons
  
  if (any(tmp <- isCircular(seqinfo(gr)))) {
    keep <- names(tmp)[!tmp]
    gr <- keepSeqlevels(gr, keep)
  }
  
  if (!is.null(keepSeqlevels)) {
    gr <- keepSeqlevels(gr, keepSeqlevels)
  }
  
  nms <- names(mcols(gr))
  geneNameIdx <- match("SYMBOL", nms)
  nms[geneNameIdx] <- "Gene"
  names(mcols(gr)) <- nms
  gr
}
