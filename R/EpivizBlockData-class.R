EpivizBlockData <- setRefClass("EpivizBlockData",
  contains="EpivizTrackData",
  methods=list(
  	initialize=function(...) {
  		callSuper(...)
  		columns <<- NULL
  	},
    plot=function(...) {
      mgr$blockChart(ms=getMeasurements(), ...)
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

setValidity2("EpivizBlockData", .valid.EpivizBlockData)

EpivizBlockData$methods(
  getMeasurements=function() {
    out <- list(list(id=id,
                name=name,
                type="range",
                datasourceId=id,
                datasourceGroup=id,
                defaultChartType="Blocks Track",
                annotation=NULL,
                minValue=NA,
                maxValue=NA,
                metadata=NULL))
      out
  },
  parseMeasurement=function(msId) {
    if (msId != id)
      stop("invalid parsed measurement")
    NULL
  },
  .getMetadata=function(curHits, metadata) {
    return(NULL)
  },
  .getValues=function(curHits, measurement) {
    return(NULL)
  }
)

