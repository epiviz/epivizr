EpivizTrackData <- setRefClass("EpivizTrackData",
  contains="EpivizData",
  methods=list(
  	initialize=function(object=GNCList(GRanges()), ...) {
	  	callSuper(object=object, ...)
	  },
	update=function(newObject, ...) {
		if(!is(newObject,"GenomicRanges"))
			stop("'newObject' must be of class 'GenomicRanges'")

                newObject <- reorderIfNeeded(newObject)
                
		if(!is(newObject, "GNCList"))
			newObject <- as(newObject, "GNCList")
		callSuper(newObject, ...)
	}
  ))

.valid.EpivizTrackData.object <- function(x) {
	if(!is(x$object, "GNCList"))
		return("'object' is not a 'GNCList' object")
	NULL
}

.valid.EpivizTrackData <- function(x) {
	c(.valid.EpivizTrackData.object(x))
}

setValidity2("EpivizTrackData", .valid.EpivizTrackData)

