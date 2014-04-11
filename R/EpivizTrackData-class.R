EpivizTrackData <- setRefClass("EpivizTrackData",
  contains="EpivizData",
  methods=list(
  	initialize=function(object=GIntervalTree(GRanges()), ...) {
	  	callSuper(object=object, ...)
	  },
	update=function(newObject, ...) {
		if(!is(newObject,"GenomicRanges"))
			stop("'newObject' must be of class 'GenomicRanges'")
			
		if(!is(newObject, "GIntervalTree"))
			newObject <- as(newObject, "GIntervalTree")
		callSuper(newObject)
	}
  ))

.valid.EpivizTrackData.object <- function(x) {
	if(!is(x$object, "GIntervalTree"))
		return("'object' is not a 'GIntervalTree' object")
	NULL
}

.valid.EpivizTrackData <- function(x) {
	c(.valid.EpivizTrackData.object(x))
}

IRanges::setValidity2("EpivizTrackData", .valid.EpivizTrackData)

