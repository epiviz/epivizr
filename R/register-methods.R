setGeneric("register", signature=c("object"), 
	function(object, columns=NULL, ...) standardGeneric("register"))

setMethod("register", "GenomicRanges",
	function(object, columns, type=c("block","bp"), ...) {
		type <- match.arg(type)
                if (S4Vectors:::isNotSorted(object)) {
                  object <- sort(object)
                }

		if (!is(object, "GIntervalTree")) {
			object <- as(object, "GIntervalTree")
		}
		dev <- switch(type,
					  block=EpivizBlockData$new(object=object, ...),
					  bp=EpivizBpData$new(object=object, columns=columns, ...))
		return(dev)
})

setMethod("register", "SummarizedExperiment",
	function(object, columns=NULL, assay=1) {
                if (S4Vectors:::isNotSorted(rowData(object))) {
                  object <- sort(object)
                }
		
		if (!is(rowData(object), "GIntervalTree")) {
			rowData(object) <- as(rowData(object), "GIntervalTree")
		}
                mcolNames <- names(mcols(rowData(object)))

		if (!("PROBEID" %in% mcolNames)) {
			rowData(object)$PROBEID <- ""
		} 
		if (!("SYMBOL" %in% mcolNames)) {
			rowData(object)$SYMBOL <- ""
		} 
		EpivizFeatureData$new(object=object, columns=columns, assay=assay)
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

		register(sumexp, columns=columns, assay=1)
})


