EpivizBpData <- setRefClass("EpivizBpData",
  contains="EpivizTrackData",
  methods=list(
    .checkColumns=function(columns) {
      all(columns %in% names(mcols(object)))
    },
    .getColumns=function() {
      names(mcols(object))
    },
    .getNAs=function() {
      if (length(columns) == 0) {
        return(integer())
      }

      naMat <- is.na(mcols(object)[,columns])
      if (!is.matrix(naMat))
        naMat <- cbind(naMat)
      which(rowSums(naMat)>0)
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
    .getLimits=function() {
      colIndex <- match(columns, colnames(mcols(object)))
      suppressWarnings(unname(sapply(colIndex, function(i) range(pretty(range(mcols(object)[,i], na.rm=TRUE))))))
    },
    plot=function(...) {
      mgr$lineChart(ms=getMeasurements(), ...)
    }
  )
)

.valid.EpivizBpData.ylim <- function(x) {
  if(!is(x$ylim, "matrix"))
    return("'ylim' must be a matrix")
  if(nrow(x$ylim) != 2)
    return("'ylim' must have two rows")
  if(ncol(x$ylim) != length(x$columns))
    return("'ylim' must have 'length(columns)' columns")
  NULL
}

.valid.EpivizBpData <- function(x) {
  c(.valid.EpivizBpData.ylim(x))
}

setValidity2("EpivizBpData", .valid.EpivizBpData)

EpivizBpData$methods(
  getMeasurements=function() {
    out <- lapply(columns, function(curCol) {
      m <- match(curCol, columns)
      list(id=curCol,
           name=curCol,
           type="feature",
           datasourceId=id,
           datasourceGroup=id,
           defaultChartType="Line Track",
           annotation=NULL,
           minValue=ylim[1,m],
           maxValue=ylim[2,m],
           metadata=NULL)
    })
    
    #out <- paste(name, columns, sep="$")
    #nms <- paste(id, columns, sep="__")
    #names(out) <- nms
    out
  },
  .getMetadata=function(curHits, metadata) {
    return(NULL)
  },
  .getValues=function(curHits, measurement, round=FALSE) {
    if(!measurement %in% columns) {
      stop("could not find measurement", measurement)
    }
    vals <- unname(mcols(object)[curHits,measurement])
    if (round) {
      vals <- round(vals, 3)
    }
    vals
  },
  parseMeasurement=function(msId) {
    column <- strsplit(msId, split="__")[[1]][2]
    if(!.checkColumns(column)) {
      stop("invalid parsed measurement")
    }
    column
  }
)

