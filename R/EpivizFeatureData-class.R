EpivizFeatureData <- setRefClass("EpivizFeatureData",
  contains="EpivizData",
  fields=list(assay="ANY",metadata="ANY"),
  methods=list(
    initialize=function(object=SummarizedExperiment(matrix(nr=0,nc=0),rowRanges=GRanges()),
                        assay=1, ...) {
      assay <<- assay
      
      callSuper(object=object, ...)
    },
    update=function(newObject, ...) {
      if (!is(newObject, "RangedSummarizedExperiment"))
        stop("'newObject' must be of class 'RangedSummarizedExperiment'")

      newObject <- reorderIfNecessary(newObject)
      
      if(!is(rowRanges(newObject), "GNCList"))
        rowRanges(newObject) <- as(rowRanges(newObject), "GNCList")
      callSuper(newObject, ...)
    },
    .checkColumns=function(columns) {
      all(columns %in% rownames(colData(object)))
    },
    .getColumns=function() {
      rownames(colData(object))
    },
    .checkLimits=function(ylim) {
      if (!is.matrix(ylim))
        return(FALSE)
      if (nrow(ylim) != 2)
        return(FALSE)
      if (ncol(ylim) != length(columns))
        return(FALSE)
      TRUE
    },
    .getNAs=function() {
      mat <- GenomicRanges::assay(object, i=.self$assay)
      colIndex <- match(columns, rownames(colData(object)))
      namat <- is.na(mat[,colIndex])
      if (!is.matrix(namat))
        namat <- cbind(namat)
      which(rowSums(namat)>0)
    },
    .getLimits=function() {
      mat <- GenomicRanges::assay(object, i=.self$assay)
      colIndex <- match(columns, rownames(colData(object)))
      unname(sapply(colIndex, function(i) range(pretty(range(mat[,i], na.rm=TRUE)))))
    },
    plot=function(x, y, ...) {
      ms <- getMeasurements()
      if (length(ms)<2)
        stop("need at least two columns to plot")

      mgr$scatterChart(x=ms[[1]], y=ms[[2]], ...)
    }
  )
)

.valid.EpivizFeatureData.object <- function(x) {
  if(!is(x$object, "RangedSummarizedExperiment"))
    return("'object' must be of class 'RangedSummarizedExperiment'")
  if(!is(rowRanges(x$object), "GNCList"))
    return("'rowRanges(object)' must be of class 'GNCList'")
  NULL
}

.valid.EpivizFeatureData.ylim <- function(x) {
  if(!is(x$ylim, "matrix"))
    return("'ylim' must be a matrix")
  if(nrow(x$ylim) != 2)
    return("'ylim' must have two rows")
  if(ncol(x$ylim) != length(x$columns))
    return("'ylim' must have 'length(columns)' columns")
  NULL
}

.valid.EpivizFeatureData.assay <- function(x) {
  if (is.character(x$assay)) {
    if(!(x$assay %in% names(assays(x$object))))
      return("'assay' not found in 'object'")
    return(NULL)
  }

  if (x$assay > length(assays(x$object)))
    return("'assay' not found in 'object'")
  NULL
}

.valid.EpivizFeatureData <- function(x) {
  c(.valid.EpivizFeatureData.object(x),
    .valid.EpivizFeatureData.ylim(x),
    .valid.EpivizFeatureData.assay(x))
}

setValidity2("EpivizFeatureData", .valid.EpivizFeatureData)

EpivizFeatureData$methods(
    getMeasurements=function() {
      out <- lapply(columns, function(curCol) {
        m <- match(curCol, columns)
        
        list(id=curCol,
           name=curCol,
           type="feature",
           datasourceId=id,
           datasourceGroup=id,
           defaultChartType="Scatter Plot",
           annotation=NULL,
           minValue=ylim[1,m],
           maxValue=ylim[2,m],
           metadata=metadata)
    })
#     out <- paste(name, columns, sep="$")
  #    nms <- paste(id, columns, sep="__")
    #  names(out) <- nms
      out
    },
    parseMeasurement=function(msId) {
      column <- strsplit(msId, split="__")[[1]][2]
      if(!.checkColumns(column)) {
        stop("invalid parsed measurement")
      }
      column
    },
    .getMetadata=function(curHits, curMetadata) {
      if (length(metadata) < 1) {
          return(NULL)
      }
      
      if(any(!curMetadata %in% metadata))
        stop("error getting metadata")

      if (length(curHits) == 0) {
        out <- lapply(curMetadata, function (x) list())
        names(out) <- curMetadata
        return(out)
      }
      out <- as.list(mcols(rowRanges(object))[curHits,curMetadata])
      names(out) <- curMetadata
      out
    },
  .getValues=function(curHits, measurement, round=FALSE) {
    if (!measurement %in% columns) {
      stop("could not find measurement", measurement)
    }
    colNames <- colnames(object)
    m <- match(measurement, colNames)
    unname(assay(object, .self$assay)[curHits, m])
  }
)

