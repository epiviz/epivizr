setGeneric("register", signature=c("object"), 
	function(object, columns=NULL, ...) standardGeneric("register"))

setGeneric("reorderIfNeeded", signature=c("object"),
           function(object, ...) standardGeneric("reorderIfNeeded"))

# TODO: add a sort check
setMethod("reorderIfNeeded", "GenomicRanges",
          function(object, ...) {
            stranded <- any(strand(object) != "*")
            if (stranded) {
              oobj <- object
              strand(object) <- "*"
            }
            if (!S4Vectors::isSorted(object)) {
              order <- order(object)
              if (stranded) {
                object <- oobj[order,]
              } else {
                object <- object[order,]
              }
            }
            return(object)
})

setMethod("reorderIfNeeded", "SummarizedExperiment",
          function(object, ...) {
            gr <- rowData(object)
            stranded <- any(strand(gr) != "*")
            if (stranded) {
              ogr <- gr
              strand(gr) <- "*"
            }
            if (!S4Vectors::isSorted(gr)) {
              order <- order(gr)
              object <- object[order,]
            }
            return(object)
})

setMethod("register", "GenomicRanges",
	function(object, columns, type=c("block","bp","geneInfo"), ...) {
		type <- match.arg(type)
                object <- reorderIfNeeded(object)

		if (!is(object, "GIntervalTree")) {
			object <- as(object, "GIntervalTree")
		}
		dev <- switch(type,
					  block=EpivizBlockData$new(object=object, ...),
					  bp=EpivizBpData$new(object=object, columns=columns, ...),
                                          geneInfo=EpivizGeneInfoData$new(object=object, ...))
		return(dev)
})

setMethod("register", "SummarizedExperiment",
	function(object, columns=NULL, assay=1, metadata=NULL) {
          object <- reorderIfNeeded(object)
		
          if (!is(rowData(object), "GIntervalTree")) {
            rowData(object) <- as(rowData(object), "GIntervalTree")
          }
          mcolNames <- names(mcols(rowData(object)))
          if (is.null(metadata) && !is.null(mcolNames)) {
            metadata <- mcolNames
          }
          if (!is.null(metadata) && any(!metadata %in% mcolNames)) {
            stop("invalid metadata")
          }
          EpivizFeatureData$new(object=object, columns=columns, assay=assay, metadata=metadata)
})

setMethod("register", "ExpressionSet",
	function(object, columns, annotation=NULL, assay="exprs") {
		if (is.null(annotation) || missing(annotation)) 
			annotation <- annotation(object)

		if (annotation != 'hgu133plus2') {
			stop("only 'hgu133plus2' affy chips supported for now")
		}
		
		# make GRanges object with appropriate info
		probeids = featureNames(object)
		annoName = paste0(annotation, ".db")

		if (!require(annoName, character.only=TRUE)) {
			stop("package '", annoName, "' is required")
		}
		
		res = suppressWarnings(select(get(annoName), keys=probeids, columns=c("SYMBOL", "CHR", "CHRLOC", "CHRLOCEND"), keytype="PROBEID"))
		dups = duplicated(res$PROBEID)
		res = res[!dups,]

		drop = is.na(res$CHR) | is.na(res$CHRLOC) | is.na(res$CHRLOCEND)
		res = res[!drop,]
		
		gr = GRanges(seqnames=paste0("chr",res$CHR),
				strand=ifelse(res$CHRLOC>0, "+","-"),
				ranges=IRanges(start=abs(res$CHRLOC), end=abs(res$CHRLOCEND)))
		
		mcols(gr)[,"SYMBOL"] = res$SYMBOL
		mcols(gr)[,"PROBEID"] = res$PROBEID

    mat <- assayDataElement(object, assay)[!drop,]
    if (missing(columns) || is.null(columns))
        columns <- colnames(mat)
    
		if (any(!(columns %in% colnames(mat))))
		  stop("'columns' not found is 'assayDataElement(object, assay)'")
		
    mat <- mat[,columns]
		colnames(mat) <- columns

    if (!all(columns %in% rownames(pData(object)))) {
      pd <- data.frame(dummy=character(length(columns)))
      rownames(pd) <- columns
    } else {
      pd <- pData(object)[columns,]
    }
		sumexp <- SummarizedExperiment(assays=SimpleList(mat),
									  rowData=gr,
									  colData=DataFrame(pd))

		register(sumexp, columns=columns, assay=1,metadata=c("PROBEID","SYMBOL"))
})

.makeGeneAnnotation <- function(object, kind=c("gene","tx"), keepSeqlevels=NULL, ...)
{
  kind <- match.arg(kind)
  gr <- genes(object, columns=c("GENEID", "SYMBOL"))
  exons <- exonsBy(object, by=kind)
  
  ids <- as.character(gr$GENEID)
  exons <- reduce(ranges(exons)[ids])
  gr$Exons <- exons
  
  if (!is.null(keepSeqlevels)) {
    gr <- keepSeqlevels(gr, keepSeqlevels)
  }
  
  nms <- names(mcols(gr))
  geneNameIdx <- match("SYMBOL", nms)
  nms[geneNameIdx] <- "Gene"
  names(mcols(gr)) <- nms
  gr
}

setMethod("register", "OrganismDb",
          function(object, kind=c("gene","tx"), keepSeqlevels=NULL, ...) {
            epivizrMsg("creating gene annotation:")
            kind <- match.arg(kind)
            gr <- .makeGeneAnnotation(object, kind, keepSeqlevels)
            args <- list(...)
            if (!is.null(args$type)) {
              register(gr, ...)
            } else {
              register(gr, type="geneInfo", ...)
            }
})            
