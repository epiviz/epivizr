EpivizDeviceMgr <- setRefClass("EpivizDeviceMgr", 
  fields=list(
    url="character",
    msList="list",
    typeMap="list",
    msIdCounter="integer",
    chartList="list",
    chartIdCounter="integer",
    activeId="character",
    chartIdMap="list",
    deviceList="list",
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
     msList <<- structure(lapply(seq_along(.typeMap), function(x) list()),names=names(.typeMap))
     chartList <<- list()
     deviceList <<- list()
     verbose <<- FALSE
     nonInteractive <<- FALSE
     callSuper(...)
   },
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

# session management methods
EpivizDeviceMgr$methods(list(
   bindToServer=function() {
     server$bindManager(.self)
   },
   finalize=function() {
     stopServer()
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
   },
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
  )
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
     server$navigate(requestId=requestId,chr=chr,start=start,end=end)
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

EpivizDeviceMgr$methods(
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
   .findMeasurements=function(msType, ms) {
      typeList <- msList[[msType]]
      allMeasurements <- lapply(typeList, "[[", "measurements")
      m <- sapply(ms, function(curMs) {
        isFound <- sapply(allMeasurements, function(x) curMs %in% x)
        if (any(isFound)) which(isFound) else NA
      })  
   },
   .checkMeasurements=function(msType, ms, sendRequest=!nonInteractive, ...) {
    if (!is.character(ms)) return(FALSE)
    if (!(msType %in% names(msList))) return(FALSE)

    m <- .findMeasurements(msType, names(ms))
    if (any(is.na(m)))
      return(FALSE)

    if (sendRequest) {
      typeList <- msList[[msType]]
      isConnected <- sapply(typeList, "[[", "connected")[m]
      all(isConnected)
    } else {
      TRUE
    }
   },
  .clearDatasourceGroupCache=function(msObj, sendRequest=!nonInteractive) {
     if(!is(msObj, "EpivizData")) {
      stop("'msObj' must be an 'EpivizData' object")
     }
     msType <- getMeasurementType(msObj)
     msIndex <- match(msObj$getId(), names(msList[[msType]]))
     if (is.na(msIndex)) 
       stop("did not find object")

     if (sendRequest) {
       callback <- function(data) {
         epivizrMsg("DatasourceGroup caches cleared", tagPrompt=TRUE)
       }
       requestId <- callbackArray$append(callback)
       request <- list(requestId=requestId,
                       type="request",
                       data=list(datasourceGroup=msIndex))
       server$sendRequest(request)
     }
     invisible()
  },
   .clearChartCaches=function(msObj, sendRequest=!nonInteractive) {
     if(!is(msObj, "EpivizData")) {
      stop("'msObj' must be an 'EpivizData' object")
     }
     msType <- getMeasurementType(msObj)
     msIndex <- match(msObj$getId(), names(msList[[msType]]))
     if (is.na(msIndex)) 
      stop("did not find object")

     if (msList[[msType]][[msIndex]]$connected) {
       chartIds <- c()
       for (chart in chartList) {
        if (is.null(chartIdMap[[chart$getId()]]))
          next

        m <- .findMeasurements(msType, names(chart$measurements))
        if (!(msIndex %in% m))
          next

        chartId <- chartIdMap[[chart$getId()]]
        if (!(chartId %in% chartIds))
          chartIds <- c(chartIds, chartId)
       }

       if (sendRequest && length(chartIds)>0) {
        callback=function(data) {
          epivizrMsg("Chart caches cleared", tagPrompt=TRUE)
        }
        requestId <- callbackArray$append(callback)
        server$clearChartCaches(requestId, chartIds)
       }
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
      slot <- sapply(msList, function(typeList) msObjId %in% names(typeList))
      if (!any(slot)) {
        stop("could not find measurement object")
      }
      slot <- which(slot)
      typeList <- msList[[slot]]
      m <- match(msObjId, names(typeList))
      typeList[[m]]$obj
   },
   rmMeasurements=function(msObj) {
    if (is.character(msObj)) {
      # passed the id instead of the object
      msObj <- .self$.getMsObject(msObj)
    }

    if (!is(msObj, "EpivizData"))
      stop("'msObj' must be an 'EpivizData' object")

    msType <- .self$getMeasurementType(msObj)
    typeList <- msList[[msType]]

    slot <- match(msObj$getId(), names(typeList))
    if (is.na(slot))
      stop("object not found")
     
    objRecord <- typeList[[slot]]
    msName <- objRecord$name
    ms <- objRecord$obj$getMeasurements()

    msList[[msType]][[msObj$getId()]] <<- NULL
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
      if (length(msList[[curType]])>0) {
        for (objRecord in msList[[curType]]) {
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
      ids <- names(ms)
      nms <- sapply(ms, "[[", "name")
      lens <- sapply(ms, function(x) length(x$obj$object))
      connected <- ifelse(sapply(ms, "[[", "connected"), "*", "")
      columns <- sapply(ms, function(x) paste0(x$obj$columns,collapse=","))

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
     if (length(msList[[curType]])>0) {
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
       
       if (length(msList[[curType]])>0) {
         for (msRecord in msList[[curType]]) {
           ms <- msRecord$obj$getMeasurements()
           for (curMs in ms) {
           for (recName in names(out)) {
             if (!is.null(curMs[[recName]])) {
               out[[recName]] <- c(out[[recName]], curMs[[recName]])
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
)

#####
# fetch data method
EpivizDeviceMgr$methods(
  .initPack=function(msType, length=0L) {
      if (!(msType %in% names(typeMap))) {
        stop("cannot find 'msType'")
      }
      get(typeMap[[msType]]$class)$new()$.initPack(length=length)
  },
  .findDatasource=function(datasource) {
    for (msType in names(typeMap)) {
      curMs <- msList[[msType]]
      if (datasource %in% names(curMs)) {
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
  },
  getData=function(measurements, chr, start, end) {
     out <- list(chr=chr,start=start,end=end) 
     query <- GRanges(chr, ranges=IRanges(start, end))

     for (typeIndex in seq_along(measurements)) {
       dataType <- names(measurements)[typeIndex]
       dataName <- gsub("Measurements","Data", dataType)
       msType <- gsub("Measurements","", dataType)
       
       out[[dataName]] <- list(start=start,end=end,chr=chr)
       curMeasurements <- measurements[[typeIndex]]
       if (length(curMeasurements)==0) {
        out[[dataName]] <- list()
        next
       }

       msMap <- .self$.findMeasurements(msType, curMeasurements)
       if (any(is.na(msMap))) {
        stop("could not find measurement")
       }
       objList <- msList[[msType]]

       dataPack <- .initPack(msType, length(msMap))

       for (msIndex in seq_along(curMeasurements)) {
        msObj <- objList[[msMap[msIndex]]]$obj
        curData <- msObj$getData(query, curMeasurements[msIndex])
        dataPack$set(curData, curMeasurements[msIndex], msIndex)
       }
       out[[dataName]] <- c(out[[dataName]], dataPack$getData())
     }
     return(out)
   }
)

# chart management methods
EpivizDeviceMgr$methods(
   addChart=function(chartObject, sendRequest=!nonInteractive, ...) {
    chartIdCounter <<- chartIdCounter + 1L
    chartId <- sprintf("epivizChart_%d", chartIdCounter)
    chartObject$setId(chartId)
    chartList[[chartId]] <<- chartObject

    if (sendRequest) {
      callback=function(data) {
        appChartId = data$id
        chartIdMap[[chartId]] <<- appChartId
        activeId <<- chartId
        epivizrMsg("Chart ", chartId, " added to browser and connected", tagPrompt=TRUE)  
      }
      requestId=callbackArray$append(callback)
      server$addChart(requestId, chartObject$type, chartObject$measurements)
    }
    invisible(NULL)
   }, 
   .getChartObject=function(chartId) {
    obj <- chartList[[chartId]]
    if (is.null(obj))
      stop("cannot find object")
    obj
   },
   rmChart=function(chartObj) {
    if (is.character(chartObj)) {
      # passed the id instead of the object
      chartObj <- .self$.getChartObject(chartObj)
    }

    if (!is(chartObj, "EpivizChart"))
      stop("'chartObj' must be an 'EpivizChart' object")

    slot <- match(chartObj$getId(), names(chartList))
    if (is.na(slot))
      stop("object not found")

    chartId <- chartObj$getId()
    chartList[[chartId]] <<- NULL

    if(!is.null(chartIdMap[[chartId]])) {
      callback=function(data) {
        epivizrMsg("chart ", chartId, " removed and disconnected", tagPrompt=TRUE)  
      }
      requestId=callbackArray$append(callback)
      server$rmChart(requestId, chartIdMap[[chartId]])
    }
    invisible(NULL)
   },
   rmAllCharts=function(which=c("noDevice", "onlyDevice", "all")) {
    which = match.arg(which)
    for (obj in chartList) {
      if ((!obj$inDevice && (which %in% c("noDevice", "all"))) ||
          (obj$inDevice && (which %in% c("onlyDevice", "all"))))
        rmChart(obj)
    }
    invisible()
   }, 
   listCharts=function() {
    if (length(chartList) == 0) {
      return(NULL)
    }
    
    ids <- names(chartList)
    type <- sapply(chartList, function(x) x$type)
    ms <- sapply(chartList, function(x) paste0(names(x$measurements), collapse=","))
    connected <- ifelse(sapply(names(chartList), function(x) x %in% names(chartIdMap)), "*", "")
    out <- data.frame(id=ids, 
                      type=type, 
                      measurements=ms, 
                      connected=connected,
                      stringsAsFactors=FALSE)
    rownames(out) <- NULL
    out
   },
   setActive=function (devId) {
     'set given device as active in browser'
     slot=which(sapply(lapply(devices,names), function(x) devId %in% x))
     if (length(slot)<1)
       stop("device Id not found")
     activeId <<- devId
     invisible(NULL)
   }
)

# device management methods
EpivizDeviceMgr$methods(
   addDevice=function(obj, devName, sendRequest=!nonInteractive, ...) {
     'add device to epiviz browser'
      deviceIdCounter <<- deviceIdCounter + 1L
      deviceId <- sprintf("epivizDevice_%d", deviceIdCounter)
 
      msObject <- .self$addMeasurements(obj, devName, sendRequest=sendRequest, ...)
      msObject$setInDevice(TRUE)

      tryCatch(waitToClearRequests(), error=function(e) {
        rmMeasurements(msObject)
        stop(e)
      })
     
     chartObject <- msObject$plot(sendRequest=sendRequest, inDevice=TRUE, ...)
     chartObject$setInDevice(TRUE)

     deviceObj <- EpivizDevice$new(msObject=msObject, chartObject=chartObject)
     deviceObj$setId(deviceId)
     deviceList[[deviceId]] <<- deviceObj
     deviceObj
   },
   # TODO: turn this into a rmMeasurement method
   rmDevice=function(deviceObj) {
     'delete device from epiviz browser'
     if (is.character(deviceObj)) {
      deviceObj <- deviceList[[deviceObj]]
      if (is.null(deviceObj)) 
        stop("did not find object")
     }

     if (!is(deviceObj, "EpivizDevice"))
      stop("'deviceObj' must be an 'EpivizDevice' object")

     devId <- deviceObj$getId()
     if (is.null(deviceList[[devId]]))
      stop("did not find obejct")

     rmChart(deviceObj$getChartObject())
     rmMeasurements(deviceObj$getMsObject())
     deviceList[[devId]] <<- NULL
     invisible()
   },
   rmAllDevices=function() {
    for (obj in deviceList) {
      rmDevice(obj)
    }
   },
  clearDeviceList=function() {
    deviceList <<- list()
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
     if (length(deviceList) == 0) {
       return(NULL)
     }
    ids <- names(deviceList)
    type <- sapply(deviceList, function(x) x$getChartObject()$type)
    ms <- sapply(deviceList, function(x) paste0(names(x$getChartObject()$measurements), collapse=","))
    connected <- ifelse(sapply(deviceList, function(x) x$getChartId() %in% names(chartIdMap)), "*", "")
    out <- data.frame(id=ids, 
                      type=type, 
                      measurements=ms, 
                      connected=connected,
                      stringsAsFactors=FALSE)
    rownames(out) <- NULL
    out

   }
)

# chart methods
EpivizDeviceMgr$methods(
  blockChart=function(ms, ...) {
    if (!.self$.checkMeasurements(msType="block", ms=ms, ...))
      stop("invalid measurements")
    
    chartObj <- EpivizChart$new(
      measurements=ms,
      mgr=.self,
      type="blocksTrack")
    addChart(chartObj, ...)
    chartObj
  },
  
  lineChart=function(ms, ...) {
    if (!.self$.checkMeasurements(msType="bp", ms=ms, ...))
      stop("invalid measurements")
    
    chartObj <- EpivizChart$new(
      measurements=ms,
      mgr=.self,
      type="lineTrack")
    addChart(chartObj, ...)
    chartObj
  },
  
  scatterChart=function(x, y, ...) {
    ms <- c(x,y)
    
    if(!.self$.checkMeasurements(msType="gene", ms=ms, ...))
      stop("invalid measurements")
    
    chartObj <- EpivizChart$new(
      measurements=ms,
      mgr=.self,
      type="geneScatterPlot")
    addChart(chartObj, ...)
    chartObj
  }
)

