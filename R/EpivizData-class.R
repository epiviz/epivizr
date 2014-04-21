EpivizData <- setRefClass("EpivizData",
  contains="VIRTUAL",
  fields=list(
    object="ANY",
    mgr="EpivizDeviceMgr",
    inDevice="logical",
    id="character",
    name="character",
    columns="ANY",
    ylim="ANY",
    curQuery="ANY",
    curHits="ANY"
  ),
  methods=list(
    initialize=function(object=GIntervalTree(GRanges()), columns=NULL, ylim=NULL, ...) {
      object <<- object
      columns <<- columns

      if (!.self$.checkColumns(columns))
        stop("Invalid 'columns' argument")

      if (is.null(.self$columns))
        columns <<- .self$.getColumns()

      if (!is.null(ylim)) {
        if (!.self$.checkLimits(ylim))
          stop("invalid 'ylim' argument")
        ylim <<- ylim
      } else {
        ylim <<- .self$.getLimits()
      }

      curQuery <<- NULL
      curHits <<- NULL
      inDevice <<- FALSE
      callSuper(...)
    },
    .checkColumns=function(columns) {
      is.null(columns)
    },
    .getColumns=function() {
      NULL
    },
    .checkLimits=function(ylim) {
      is.null(ylim)
    },
    .getLimits=function() {
      NULL
    },
    update=function(newObject, sendRequest=TRUE) {
      if(class(newObject) != class(object)) {
        stop("class of 'newObject' is not equal to class of current 'object'")
      }

      oldObject <- object
      object <<- newObject

      if (!is.null(columns)) {
        if (!.checkColumns(columns)) {
          object <<- oldObject
          stop("columns not found in 'newObject'")
        }

        ylim <<- .getLimits()
      }
      if (sendRequest && !is.null(mgr)) {
        mgr$.clearDatasourceGroupCache(.self, sendRequest=sendRequest)
#        mgr$.clearChartCaches(.self, sendRequest=sendRequest)
      }

      invisible()
    },
    getId=function() {
      return(id)
    },
    setId=function(id) {
      id <<- id
      invisible()
    },
    getName=function() {return(name)},
    setName=function(name) {
      name <<- name
      invisible()
    },
    setLimits=function(ylim) {
      if (!.checkLimits(ylim))
          stop("'invalid' limits argument")
      ylim <<- ylim
    }, 
    getMeasurements=function() {
      stop("'getMeasurements' called on virtual class object")
    },
    parseMeasurement=function(msId=NULL) {
      stop("'parseMeasurement' called on virtual class object")
    },
    setMgr=function(mgr) {
      if (!is(mgr, "EpivizDeviceMgr"))
        stop("'mgr' must be of class 'EpivizDeviceMgr'")
      
      mgr <<- mgr
      invisible()
    },
    setInDevice=function(x) {
      if (!is.logical(x))
        stop("'x' must be 'logical'")
      inDevice <<- x
      invisible()
    },
    show=function() {
      cat(class(.self), "object", id, "\n")
      methods::show(object)
      cat("\n\tcolumns:", paste(columns,collapse=","),"\n")
      cat("\tlimits:\n")
      print(ylim)
    },
    plot=function() {
      stop("'plot' method called on virtual class object")
    }
  )
)

#####
# validity
.valid.EpivizData.columns <- function(x) {
  if(!x$.checkColumns(x$columns))
    return("invalid 'columns' slot")
  NULL
}

.valid.EpivizData <- function(x) {
  c(.valid.EpivizData.columns(x))
}

setValidity2("EpivizData", .valid.EpivizData)

#######
# get data
EpivizData$methods(
  packageData=function(msId) {
    stop("'packageData' called on object of virtual class")
  },
  getData=function(query, msId=NULL) {
    if (!is(query, "GRanges"))
      stop("'query' must be a GRanges object")
    if (length(query) != 1) {
      stop("'query' must be of length 1")
    }

    if (is.null(curQuery) || !identical(unname(query), unname(curQuery))) {
      curQuery <<- query
      olaps <- GenomicRanges::findOverlaps(query, object, select="all")
      curHits <<- subjectHits(olaps)
      if (S4Vectors:::isNotSorted(start(object)[curHits])) {
        ord <- order(start(object)[curHits])
        curHits <<- curHits[ord]
      }
    }
    packageData(msId=msId)
  }
)

EpivizDataPack <- setRefClass("EpivizDataPack",
  fields=list(
    length="integer"),
  methods=list(
    initialize=function(length=0L, ...) {
      length <<- length
      callSuper(...)
    },
    set=function(curData, msId, index) {
      step("calling 'set' on virtual class")
    },
    getData=function() {
      stop("calling 'getData' on virtual class")
    }
  )
)


