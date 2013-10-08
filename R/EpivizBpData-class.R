EpivizBpData <- setRefClass("EpivizBpData",
  contains="EpivizTrackData",
  methods=list(
    .checkColumns=function(columns) {
      all(columns %in% names(mcols(object)))
    },
    .getColumns=function() {
      names(mcols(object))
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
      sapply(mcols(object)[columns], function(x) range(pretty(range(x, na.rm=TRUE))))
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

IRanges::setValidity2("EpivizBpData", .valid.EpivizBpData)

EpivizBpData$methods(
  getMeasurements=function() {
    out <- paste(name, columns, sep="$")
    nms <- paste(id, columns, sep="$")
    names(out) <- nms
    out
  },
  parseMeasurement=function(msId) {
    column <- strsplit(msId, split="\\$")[[1]][2]
    if(!.checkColumns(column)) {
      stop("invalid parsed measurement")
    }
    column
  },
  packageData=function(msId) {
    column <- parseMeasurement(msId)
    m <- match(column, columns)
    out <- list(min=unname(ylim[1,m]), max=unname(ylim[2, m]))
    out$data <- list(bp=integer(), value=numeric())

    if (length(curHits)) {
      vals <- mcols(object)[curHits,column]
      naIndx <- is.na(vals)
      if (!all(naIndx)) {
        if (any(naIndx)) {
          out$data <- list(bp=start(object)[curHits[!naIndx]], value=vals[!naIndx])  
        } else {
          out$data <- list(bp=start(object)[curHits], value=vals)
        }
      }
    }
    out
  }
)

EpivizBpDataPack <- setRefClass("EpivizBpDataPack",
  contains="EpivizDataPack",
  fields=list(
    min="numeric",
    max="numeric",
    data="list"),
  methods=list(
    initialize=function(...) {
      callSuper(...)
      min <<- structure(rep(-6, length), names=rep("", length))
      max <<- structure(rep(6, length), names=rep("", length))
      data <<- lapply(seq(len=length), function(i) list(bp=integer(), value=numeric()))
      names(data) <<- rep("", length)
    },
    set=function(curData, msId, index) {
      min[index] <<- curData$min
      max[index] <<- curData$max
      names(min)[index] <<- msId
      names(max)[index] <<- msId
      data[[index]] <<- curData$data
      names(data)[index] <<- msId
    },
    getData=function() {
      list(min=min,max=max,data=data)
    }
  ))
EpivizBpData$methods(.initPack=function(length=0L) {EpivizBpDataPack$new(length=length)})
