EpivizDeviceMgr <- setRefClass("EpivizDeviceMgr", 
  fields=list(
    url="character",
    msList="list",
    typeMap="list",
    msIdCounter="integer",
    chartList="environment",
    chartIdCounter="integer",
    activeId="character",
    chartIdMap="list",
    deviceList="environment",
    deviceIdCounter="integer",
    server="EpivizServer",
    verbose="logical",
    nonInteractive="logical",
    callbackArray="IndexedArray"),
  methods=list(
    initialize=function(...) {
     msIdCounter <<- 0L
     chartIdCounter <<- 0L
     deviceIdCounter <<- 0L
     activeId <<- ""
     chartIdMap <<- list()
     typeMap <<- .typeMap
#     msList <<- list()
     msList <<- structure(lapply(seq_along(.typeMap), function(x) new.env()),names=names(.typeMap))
     chartList <<- new.env()
     deviceList <<- new.env()
     verbose <<- FALSE
     nonInteractive <<- FALSE
     callSuper(...)
   },
#   finalize=function() {
#     stopServer()
#   },
   show=function() {
      cat("Epiviz device manager object:\n")
      cat("Server: ")
      server$show(); cat("\n")
      
      st <- .self$listCharts()
      if (!is.null(st)) {
        cat("Charts:\n")
        print(st); cat("\n")
      }
      
      st <- .self$listMeasurements()
      if (length(st)>0) {
        cat("Measurements:\n")
        print(st); cat("\n")
      }
      
      st <- .self$listDevices()
      if (!is.null(st)) {
        cat("Devices:\n")
        print(.self$listDevices()); cat("\n")
      }
#      listTypes()
   }
  )
)

EpivizDeviceMgr$methods(list(
  getSeqInfos=function() {
    return(list())
  })
)

# daemonization helpers
EpivizDeviceMgr$methods(list(
  daemonized=function() server$daemonized,
  waitToClearRequests=function(timeout=3L) {
    ptm <- proc.time()
    while (server$requestWaiting && (proc.time() - ptm < timeout)["elapsed"]) {
      Sys.sleep(0.001)
      service()
    }
    if (server$requestWaiting) {
      stop("requests not cleared")
    }
    return(invisible())
  }
))

# measurement management methods
.typeMap <- list(gene=list(class="EpivizFeatureData",
                           description="Data indexed by feature",
                           input_class="SummarizedExperiment"),
              bp=list(class="EpivizBpData",
                      description="Basepair resolution data",
                      input_class="GRanges"),
              block=list(class="EpivizBlockData",
                         description="Genomic region data",
                         input_class="GRanges"))

EpivizDeviceMgr$methods(list(
   addMeasurements=function(obj, msName, sendRequest=!nonInteractive, ...) {
    'add measurements to epiviz session'
    epivizObject <- epivizr:::register(obj, ...)
    type <- .self$getMeasurementType(epivizObject)

    msIdCounter <<- msIdCounter + 1L
    msId <- sprintf("epivizMs_%s_%d", type, msIdCounter)
    epivizObject$setId(msId)
    epivizObject$setName(msName)
    epivizObject$setMgr(.self)

    measurements <- epivizObject$getMeasurements()
    msRecord <- list(measurements=measurements, 
      name=msName, obj=epivizObject, connected=FALSE)
    msList[[type]][[msId]] <<- msRecord

    if (sendRequest) {
      callback <- function(data) {
        msList[[type]][[msId]][["connected"]] <<- TRUE
        epivizrMsg("Measurement ", msName, " added to browser and connected", tagPrompt=TRUE)
      }
      requestId <- callbackArray$append(callback)
      request <- list(requestId=requestId,
                      type="request")
      request$data <- list(action="addMeasurements",
                           measurements=rjson::toJSON(measurements))
      server$sendRequest(request)
    }
    return(epivizObject)
   },
#    .findMeasurements=function(msType, ms) {
#       typeList <- msList[[msType]]
#       ids <- ls(typeList)
#       allMeasurements <- lapply(ids, function(id) typeList[[id]]$measurements)
#       m <- sapply(ms, function(curMs) {
#         isFound <- sapply(allMeasurements, function(x) curMs %in% x)
#         if (any(isFound)) which(isFound) else NA
#       })  
#    },
#    .checkMeasurements=function(msType, ms, sendRequest=!nonInteractive, ...) {
#     if (!is.character(ms)) return(FALSE)
#     if (!(msType %in% names(msList))) return(FALSE)
# 
#     m <- .findMeasurements(msType, names(ms))
#     if (any(is.na(m)))
#       return(FALSE)
# 
#     if (sendRequest) {
#       typeList <- msList[[msType]]
#       isConnected <- sapply(typeList, "[[", "connected")[m]
#       all(isConnected)
#     } else {
#       TRUE
#     }
#    },
  .clearDatasourceGroupCache=function(msObj, sendRequest=!nonInteractive) {
     if(!is(msObj, "EpivizData")) {
      stop("'msObj' must be an 'EpivizData' object")
     }
     msType <- getMeasurementType(msObj)
     if (!exists(msObj$getId(), envir=msList[[msType]], inherits=FALSE))
       stop("did not find object")

     if (sendRequest) {
       callback <- function(data) {
         epivizrMsg("DatasourceGroup caches cleared", tagPrompt=TRUE)
       }
       callback2 <- function(data) {
         epivizrMsg("Redrawn", tagPrompt=TRUE)
       }
       
       requestId <- callbackArray$append(callback)
       request <- list(requestId=requestId,
                       type="request",
                       data=list(action="clearDatasourceGroupCache",
                         datasourceGroup=msObj$getId()))
       server$sendRequest(request)

       requestId <- callbackArray$append(callback2)
       request <- list(requestId=requestId,
                       type="request",
                       data=list(action="redraw"))
       server$sendRequest(request)
     }
     invisible()
  },
   updateMeasurements=function(oldObject, newObject, sendRequest=!nonInteractive) {
     if (is.character(oldObject))
       oldObject <- .getMsObject(oldObject)
     if (!is(oldObject, "EpivizData"))
      stop("oldObject must be of class 'EpivizData'")
     oldObject$update(newObject, sendRequest=sendRequest)
     invisible()
   },
   .getMsObject=function(msObjId) {
      slot <- sapply(msList, function(typeList) exists(msObjId, typeList, inherits=FALSE))
      if (!any(slot)) {
        stop("could not find measurement object")
      }
      slot <- which(slot)
      typeList <- msList[[slot]]
      msRecord <- get(msObjId, envir=typeList, inherits=FALSE)
      msRecord$obj
   },
   rmMeasurements=function(msObj) {
    if (is.character(msObj)) {
      # passed the id instead of the object
      msObj <- .self$.getMsObject(msObj)
    }

    if (!is(msObj, "EpivizData")) {
      browser()
      stop("'msObj' must be an 'EpivizData' object")
    }

    msType <- .self$getMeasurementType(msObj)
    typeList <- msList[[msType]]

    if (!exists(msObj$getId(), envir=typeList, inherits=FALSE))
      stop("object not found")
     
    objRecord <- get(msObj$getId(), typeList, inherits=FALSE)
    msName <- objRecord$name
    ms <- objRecord$obj$getMeasurements()

    rm(list=msObj$getId(), envir=msList[[msType]])
    if(objRecord$connected) {
      callback=function(data) {
        epivizrMsg("measurement object ", msName, " removed and disconnected", tagPrompt=TRUE)  
      }
      requestId=callbackArray$append(callback)
      request <- list(requestId=requestId,
                      type="request")
      request$data <- list(action="removeMeasurements",
                           measurements=rjson::toJSON(ms))
      server$sendRequest(request)
    }
    invisible(NULL)
   },
   rmAllMeasurements=function(which=c("noDevice", "onlyDevice", "all")) {
    which <- match.arg(which)
    for (i in seq_along(msList)) {
      curType=names(msList)[i]
      ids <- ls(msList[[curType]])
      if (length(ids)>0) {
        for (id in ids) {
          objRecord <- msList[[curType]][[id]]
          if((!objRecord$obj$inDevice && (which %in% c("noDevice", "all")) ||
             (objRecord$obj$inDevice && (which %in% c("onlyDevice", "all")))))
            rmMeasurements(objRecord$obj)
        }
      }
    }
   },
   listMeasurements=function(onlyLocal=TRUE) {
    if (!onlyLocal) {
      stop("'onlyLocal=FALSE' not implemented yet")
    }

    .doOneList <- function(ms) {
      ids <- ls(ms)
      nms <- sapply(ids, function(id) ms[[id]]$name)
      lens <- sapply(ids, function(id) length(ms[[id]]$obj$object))
      connected <- ifelse(sapply(ids, function(id) ms[[id]]$connected), "*", "")
      columns <- sapply(ids, function(id) paste0(ms[[id]]$obj$columns,collapse=","))

      data.frame(id=ids,
                 name=nms,
                 length=lens,
                 connected=connected,
                 columns=columns,
                 stringsAsFactors=FALSE,row.names=NULL)  
   }
   out <- list()
   for (i in seq_along(msList)) {
     curType=names(msList)[i]
     if (length(ls(msList[[curType]])>0)) {
       out[[curType]] <- .doOneList(msList[[curType]])
     } else {
       out[[curType]] <- NULL
     }
   }
   return(out)
   },
   getMeasurements=function() {
     out <- list(id=character(),
                 name=character(),
                 type=character(),
                 datasourceId=character(),
                 datasourceGroup=character(),
                 defaultChartType=character(),
                 annotation=list(),
                 minValue=numeric(),
                 maxValue=numeric(),
                 metadata=list()
                 )
     for (i in seq_along(.typeMap)) {
       curType <- names(.typeMap)[i]
       nm <- paste0(curType,"Measurements")
       measurements <- list()
       
       ids <- ls(msList[[curType]])
       if (length(ids)>0) {
         for (id in ids) {
           msRecord <- msList[[curType]][[id]]
           ms <- msRecord$obj$getMeasurements()
           for (curMs in ms) {
           for (recName in names(out)) {
             if (is.list(out[[recName]])) {
               curVal <- list(curMs[[recName]])
             } else {
               curVal <- curMs[[recName]]
             }
             if (!is.null(curMs[[recName]])) {
               out[[recName]] <- c(out[[recName]], curVal)
             } else {
               out[[recName]] <- c(out[[recName]], list(NULL))
             }
           }
         }
         }
       }
     }

     if (length(out$id)==1) {
       for (recName in names(out)) {
         out[[recName]] <- list(out[[recName]])
       }
     }
     return(out)
   },
  getMeasurementType=function(x) {
    if (!is.character(x)) {
      if (!is(x, "EpivizData")) {
        stop("'x' must be 'character' or an 'EpivizData' object")
      }
      x <- class(x)
    }
    m <- match(x, sapply(typeMap, "[[", "class"))
    if (is.na(m))
      stop("Class ", x, " not found in 'typeMap'")
    names(typeMap)[m]
  }
))

#####
# fetch data method
EpivizDeviceMgr$methods(list(
  .findDatasource=function(datasource) {
    for (msType in names(typeMap)) {
      curMs <- msList[[msType]]
      if (exists(datasource, curMs, inherits=FALSE)) {
        return(curMs[[datasource]]$obj)
      }
    }
    return(NULL)
  },
  getRows=function(chr, start, end, metadata, datasource) {
     query <- GRanges(chr, ranges=IRanges(start, end))
     obj <- .findDatasource(datasource)
     if (is.null(obj)) {
       stop("cannot find datasource", datasource)
     }
     obj$getRows(query, metadata)
  },
  getValues=function(chr, start, end, datasource, measurement) {
    query <- GRanges(chr,ranges=IRanges(start,end))
    obj <- .findDatasource(datasource)
    if (is.null(obj)) {
      stop("cannot find datasource", datasource)
    }
    obj$getValues(query, measurement)
  }
))

# chart management methods
EpivizDeviceMgr$methods(list(
   addChart=function(chartObject, sendRequest=!nonInteractive, ...) {
    chartIdCounter <<- chartIdCounter + 1L
    chartId <- sprintf("epivizChart_%d", chartIdCounter)
    chartObject$setId(chartId)
    chartList[[chartId]] <<- chartObject

    if (sendRequest) {
      callback=function(data) {
        appChartId = data$value$id
        chartIdMap[[chartId]] <<- appChartId
        activeId <<- chartId
        epivizrMsg("Chart ", chartId, " added to browser and connected", tagPrompt=TRUE)  
      }
      requestId=callbackArray$append(callback)
      request <- list(type="request",
                      requestId=requestId,
                      data=list(action="addChart",
                        type=chartObject$type,
                        measurements=rjson::toJSON(chartObject$measurements)))
      server$sendRequest(request)
    }
    invisible(NULL)
   }, 
   .getChartObject=function(chartId) {
     if (!exists(chartId, envir=chartList, inherits=FALSE))
      stop("cannot find object")
    get(chartId, envir=chartList, inherits=FALSE)
   },
   rmChart=function(chartObj) {
    if (is.character(chartObj)) {
      # passed the id instead of the object
      chartObj <- .self$.getChartObject(chartObj)
    }

    if (!is(chartObj, "EpivizChart"))
      stop("'chartObj' must be an 'EpivizChart' object")

    if(!exists(chartObj$getId(), envir=chartList, inherits=FALSE))
      stop("object not found")

    chartId <- chartObj$getId()
    rm(list=chartId, envir=chartList)
    
    if(!is.null(chartIdMap[[chartId]])) {
      callback=function(data) {
        epivizrMsg("chart ", chartId, " removed and disconnected", tagPrompt=TRUE)  
      }
      requestId=callbackArray$append(callback)
      request <- list(type="request",
                      requestId=requestId,
                      data=list(action="removeChart",
                        chartId=chartIdMap[[chartId]]))
      server$sendRequest(request)
    }
    invisible(NULL)
   },
   rmAllCharts=function(which=c("noDevice", "onlyDevice", "all")) {
    which <- match.arg(which)
    ids <- ls(chartList)
    for (id in ids) {
      obj <- chartList[[id]]
      if ((!obj$inDevice && (which %in% c("noDevice", "all"))) ||
          (obj$inDevice && (which %in% c("onlyDevice", "all"))))
        rmChart(obj)
    }
    invisible()
   }, 
   listCharts=function() {
     ids <- ls(chartList)
    if (length(ids) == 0) {
      return(NULL)
    }
    
    type <- sapply(ids, function(x) chartList[[x]]$type)
    ms <- sapply(ids,
                 function(x) {
                   tmp <- sapply(chartList[[x]]$measurements, function(y) paste0(y$datasourceId,":",y$name))
                   paste0(tmp, collapse=",")
               })
    connected <- ifelse(sapply(ids, function(x) x %in% names(chartIdMap)), "*", "")
    out <- data.frame(id=ids, 
                      type=type, 
                      measurements=ms, 
                      connected=connected,
                      stringsAsFactors=FALSE)
    rownames(out) <- NULL
    out
   }
#    setActive=function (devId) {
#      'set given device as active in browser'
#      slot=which(sapply(lapply(devices,names), function(x) devId %in% x))
#      if (length(slot)<1)
#        stop("device Id not found")
#      activeId <<- devId
#      invisible(NULL)
#    }
))

# device management methods
EpivizDeviceMgr$methods(list(
   addDevice=function(obj, devName, sendRequest=!nonInteractive, ...) {
     'add device to epiviz browser'
      deviceIdCounter <<- deviceIdCounter + 1L
      deviceId <- sprintf("epivizDevice_%d", deviceIdCounter)
 
      msObject <- .self$addMeasurements(obj, devName, sendRequest=sendRequest, ...)
      msObject$setInDevice(TRUE)

      tryCatch({
        waitToClearRequests()
        chartObject <- msObject$plot(sendRequest=sendRequest, inDevice=TRUE, ...)
        chartObject$setInDevice(TRUE)
      }, 
      error=function(e) {
        rmMeasurements(msObject)
        stop(e)
      })
      tryCatch({
        waitToClearRequests()
        deviceObj <- EpivizDevice$new(msObject=msObject, chartObject=chartObject)
        deviceObj$setId(deviceId)
        deviceList[[deviceId]] <<- deviceObj
        deviceObj
      }, 
      error=function(e) {
        rmChart(chartObject)
        rmMeasurements(msObject)
        stop(e)
      })
   },
   rmDevice=function(deviceObj) {
      'delete device from epiviz browser'
      if (is.character(deviceObj)) {
        if (!exists(deviceObj, envir=deviceList, inherits=FALSE)) {
          stop("did not find object", deviceObj) 
        }
        deviceObj <- deviceList[[deviceObj]]
      }
      
      if (!is(deviceObj, "EpivizDevice"))
        stop("'deviceObj' must be an 'EpivizDevice' object")

      devId <- deviceObj$getId()
      if (!exists(devId, envir=deviceList, inherits=FALSE))
        stop("did not find obejct")

      rmChart(deviceObj$getChartObject())
      rmMeasurements(deviceObj$getMsObject())
      rm(list=devId, envir=deviceList)
      invisible()
   },
   rmAllDevices=function() {
     devIds <- ls(deviceList)
     for (devId in devIds) {
       rmDevice(devId)
     }
   },
  clearDeviceList=function() {
    deviceList <<- new.env()
    invisible()
  },
  updateDevice=function(oldObject, newObject, sendRequest=!nonInteractive) {
     if (is.character(oldObject))
       oldObject <- deviceList[[oldObject]]

     if (!is(oldObject, "EpivizDevice"))
      stop("oldObject must be of class 'EpivizDevice'")

     oldObject$update(newObject, sendRequest=sendRequest)
     invisible()
   },
   listDevices=function() {
     'list devices in browser'
     ids <- ls(deviceList)
     if (length(ids) == 0) {
       return(NULL)
     }
    type <- sapply(ids, function(x) deviceList[[x]]$getChartObject()$type)
    ms <- sapply(ids, function(x) {
       tmp <- sapply(deviceList[[x]]$getChartObject()$measurements, function(y) paste0(y$datasourceId,":",y$name))
       paste0(tmp,collapse=",")
     })
   
    connected <- ifelse(sapply(ids, function(x) deviceList[[x]]$getChartId() %in% names(chartIdMap)), "*", "")
    out <- data.frame(id=ids, 
                      type=type, 
                      measurements=ms, 
                      connected=connected,
                      stringsAsFactors=FALSE)
    rownames(out) <- NULL
    out

   }
))

# session management methods
EpivizDeviceMgr$methods(list(
  bindToServer=function() {
    server$bindManager(.self)
  },
  isClosed=function() {
    'check if connection is closed'
    server$isClosed()
  },
  openBrowser=function(url=NULL) {
    closeOnError <- FALSE
    if (server$isClosed()) {
      closeOnError <- TRUE
      if (verbose) {
        epivizrMsg("Starting epiviz websocket connection")
      }
      tryCatch(server$startServer(),
               error=function(e) stop(e))
    }
    
    if (nonInteractive) {
      return(invisible())
    }
    
    if (verbose) {
      epivizrMsg("Opening browser")
    }
    
    tryCatch({
      if (missing(url) || is.null(url)) {
        browseURL(.self$url)
      } else {
        browseURL(url)
      }
      
      if (daemonized())
        return(invisible())
      
      if (verbose) {
        epivizrMsg("Servicing websocket until connected")
      }
      
      ptm <- proc.time()
      while(!server$socketConnected && (proc.time()-ptm)[2] * 1000 <= 30) {
        service(verbose=FALSE)
      }
      if (!server$socketConnected) {
        stop("[epivizr] Error opening connection. UI unable to connect to websocket server.")
      }
    }, error=function(e) {
      if (closeOnError) server$stopServer()
      stop(e)
    })
  },
  service=function(verbose=TRUE) {
    if (verbose && !(nonInteractive || daemonized())) {
      epivizrMsg("Serving Epiviz, escape to continue interactive session...")
    }
    
    server$service(nonInteractive)
  },
  stopService=function() {
    server$stopService()
  },
  startServer=function() {
    epivizrMsg("Starting websocket server...")
    server$startServer()
  },
  stopServer=function() {
    'stop epiviz connection'
    .self$rmAllCharts(which="all")
    .self$waitToClearRequests()
    .self$rmAllMeasurements(which="all")
    .self$waitToClearRequests()
    .self$clearDeviceList()
    server$stopServer()
    invisible()
  })
)

# navigation methods
EpivizDeviceMgr$methods(list(
  refresh=function() {
    'refresh browser'
    server$refresh()
  },
  navigate=function(chr, start, end) {
    'navigate to given position'
    callback <- function(data) {
      invisible(NULL)
    }
    requestId <- callbackArray$append(callback)
    request=list(type="request",
                 requestId=requestId,
                 data=list(action="navigate",
                           range=rjson::toJSON(list(seqName=chr,start=start,end=end))))
    server$sendRequest(request)
  },
  slideshow=function(granges, n=10) {
    'navidate to successive positions'
    if (!is(granges, "GenomicRanges"))
      stop(("'granges' must be a 'GenomicRanges' object"))
    
    ind <- seq(len=n)
    chr <- as.character(seqnames(granges)[ind])
    start <- start(granges)[ind]
    end <- end(granges)[ind]
    for (i in ind) {
      cat("Region", i, "of", n, ". Press key to continue (ESC to stop)...\n")
      if (!nonInteractive)
        readLines(n=1)
      navigate(chr=chr[i], start=start[i], end=end[i])
      tryCatch(service(), interrupt=function(int) invisible(NULL))
    }
    invisible(NULL)
  }
)
)

# chart methods
EpivizDeviceMgr$methods(list(
  blockChart=function(ms, ...) {
#    if (!.self$.checkMeasurements(msType="block", ms=ms, ...))
 #     stop("invalid measurements")
    
    chartObj <- EpivizChart$new(
      measurements=ms,
      mgr=.self,
      type="epiviz.plugins.charts.BlocksTrack")
    addChart(chartObj, ...)
    chartObj
  },
  
  lineChart=function(ms, ...) {
#    if (!.self$.checkMeasurements(msType="bp", ms=ms, ...))
 #     stop("invalid measurements")
    
    chartObj <- EpivizChart$new(
      measurements=ms,
      mgr=.self,
      type="epiviz.plugins.charts.LineTrack")
    addChart(chartObj, ...)
    chartObj
  },
  
  scatterChart=function(x, y, ...) {
    ms <- list(x,y)
    
#    if(!.self$.checkMeasurements(msType="gene", ms=ms, ...))
 #     stop("invalid measurements")
    
    chartObj <- EpivizChart$new(
      measurements=ms,
      mgr=.self,
      type="epiviz.plugins.charts.ScatterPlot")
    addChart(chartObj, ...)
    chartObj
  },

  heatmapChart=function(ms, ...) {
    chartObj <- EpivizChart$new(
                  measurements=ms,
                  mgr=.self,
                  type="epiviz.plugins.charts.HeatmapPlot")
    addChart(chartObj, ...)
    chartObj
  }
))

