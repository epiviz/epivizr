EpivizBlockData <- setRefClass("EpivizBlockData",
  contains="EpivizTrackData",
  methods=list(
  	initialize=function(...) {
  		callSuper(...)
  		columns <<- NULL
  	},
    plot=function(...) {
      mgr$blockChart(ms=getMeasurements()[1], ...)
    }
  )
)

.valid.EpivizBlockData.ylim <- function(x) {
	if (!is.null(x$ylim))
		return("'ylim' must be 'NULL'")
	NULL
}

.valid.EpivizBlockData <- function(x) {
	c(.valid.EpivizBlockData.ylim(x))
}

IRanges::setValidity2("EpivizBlockData", .valid.EpivizBlockData)

EpivizBlockData$methods(
  getMeasurements=function() {
      out <- name
      names(out) <- id
      out
  },
  parseMeasurement=function(msId) {
    if (msId != id)
      stop("invalid parsed measurement")
    NULL
  },
  packageData=function(msId=NULL) {
    if (!length(curHits)) {
      out <- list(start=integer(), end=integer())
    } else {
      out <- list(start=start(object)[curHits], end=end(object)[curHits])
    }
  }
)

EpivizBlockDataPack <- setRefClass("EpivizBlockDataPack",
  contains="EpivizDataPack",
  fields=list(data="list"),
  methods=list(
    initialize=function(...) {
      callSuper(...)
      data <<- structure(vector("list",length), names=rep("", length))
    },
    set=function(curData, msId, index=1) {
      if(index > length) 
        stop("cannot set to 'index'")
      data[[index]] <<- curData
      names(data)[index] <<- msId
    },
    getData=function() {list(data=data)}
  )
)
EpivizBlockData$methods(.initPack=function(length=0) {EpivizBlockDataPack$new(length=length)})
