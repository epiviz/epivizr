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
  mdata <- mcols(x$object)
  nms <- names(mdata)
  requiredNames <- c("Gene","Exons")
  if (any(!requiredNames %in% nms))
    return("'metadata' must contain columns 'Gene' and 'Exons'")

  if (is(mdata$Gene, "Rle") && !is.character(runValue(mdata$Gene)))
    return("'Gene' must be a 'character' vector or Rle, or 'CharacterList'")
  if (!is(mdata$Gene, "Rle") && !(is.character(mdata$Gene) || is(mdata$Gene, "CharacterList")))
    return("'Gene' must be a 'character' vector or Rle, or 'CharacterList'")
  
  if (!is(mdata$Exons, "IRangesList"))
    return("'Exons' must be an 'IRangesList'")

  NULL
}

.valid.EpivizGeneInfoData <- function(x) {
  c(.valid.EpivizGeneInfoData.ylim(x),
    .valid.EpivizGeneInfoData.metadata(x))
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
  getRows=function(query, metadata) {
    out <- callSuper(query, metadata)
    if (length(curHits) == 0) {
      return(out)
    }

    out$values$strand <- as.character(strand(object)[curHits])
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
