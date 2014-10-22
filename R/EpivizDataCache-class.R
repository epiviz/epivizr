EpivizDataCache <- setRefClass("EpivizDataCache",
  contains="VIRTUAL",                               
  field=list(
    resource="ANY",
    cacheRange="GRanges"),
  methods=list(
    initialize=function(cacheRange=GRanges(), ...) {
      cacheRange <<- cacheRange
      callSuper(...)
    },
    getObject=function(query) {
      stop("calling 'getObject' on virtual class")
    }
  )
)                               
