EpivizGeneInfoData <- setRefClass("EpivizGeneInfoData",
  contains="EpivizTrackData",
  methods=list(
    initialize=function(...) {
      callSuper(...)
      columns <<- NULL
    },
    plot=function(...) {
      mgr$genesChart(ms=getMeasurements(), ...)
    }
  )
)

.valid.EpivizGeneInfoData.ylim <- function(x) {
  if (!is.null(x$ylim))
    return("'ylim' must be 'NULL'")
  NULL
}

.valid.EpivizGeneInfoData.metadata <- function(x) {
  nms <- mcols(x)
  requiredNames <- c("Gene","Exons")
  if (any(!requireNames %in% nms))
    return("'metadata' must contain columns 'Gene' and 'Exons'")

  if (is(x$Gene, "Rle") && !is.character(runValue(x$Gene)))
    return("'Gene' must be a 'character' vector or Rle")
  if (!is(x$Gene, "Rle") && !is.character(x$Gene))
    return("'Gene' must be a 'character' vector or Rle")

  if (!is(x$Exons, "IRangesList"))
    return("'Exons' must be an 'IRangesList'")

  NULL
}

.valid.EpivizGeneInfoData <- function(x) {
  c(.valid.EpivizGeneInfoData.ylim(x))
}

setValidity2("EpivizGeneInfoData", .valid.EpivizGeneInfoData)

EpivizGeneInfoData$methods(
  getMeasurements=function() {
    out <- list(list(id=id,
                     name=name,
                     type="range",
                     datasourceId=id,
                     datasourceGroup=id,
                     defaultChartType="Genes Track",
                     annotation=NULL,
                     minValue=NA,
                     maxValue=NA,
                     metadata=c("gene", "exon_starts","exon_ends")))
    out
  },
  .getMetadata=function(curHits, curMetadata) {
    if (length(curHits) == 0) {
      out <- lapply(curMetadata, function(x) list())
      return(out)
    }
    out <- vector("list", length(curMetadata))
    names(out) <- curMetadata
    for (col in curMetadata) {
      curOut <- switch(col,
                       gene=as.character(object$Gene[curHits]),
                       exon_starts=unname(lapply(start(object$Exons)[curHits], paste, collapse=",")),
                       exon_ends=unname(lapply(end(object$Exons)[curHits],paste,collapse=",")))
      out[[col]] <- curOut
    }
    out
  },
  .getValues=function(curHits, measurement) {
    return(NULL)
  }
)  
