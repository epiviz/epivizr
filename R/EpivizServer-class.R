EpivizServer <- setRefClass("EpivizServer",
  fields=list(
    port="integer",
    websocket="ANY",
    server="ANY",
    interrupted="logical",
    socketConnected="logical",
    verbose="logical",
    msgCallback="function",
    requestQueue="Queue",
    requestWaiting="logical",
    tryPorts="logical",
    daemonized="logical",
    standalone="logical",
    startServerFn="function",
    stopServerFn="function"
  ),
  methods=list(
    initialize=function(port=7312L, tryPorts=FALSE, daemonized=NULL, standalone=NULL, verbose=FALSE, ...) {
      port <<- port
      interrupted <<- FALSE
      socketConnected <<- FALSE
      server <<- NULL
      tryPorts <<- tryPorts
      requestWaiting <<- FALSE
      daemonized <<-  .epivizrCanDaemonize() && isTRUE(daemonized)
      startServerFn <<- if (.self$daemonized) httpuv::startDaemonizedServer else httpuv::startServer
      stopServerFn <<- if (.self$daemonized) httpuv::stopDaemonizedServer else httpuv::stopServer
      standalone <<- isTRUE(standalone)
      verbose <<- verbose
      callSuper(...)
    },
    finalize=function() {
      stopServer()
    },
    tryMorePorts=function(callbacks,minPort=7000L, maxPort=7999L) {
      success <- FALSE
      port <<- minPort
      while(!success && port <= maxPort) {
        tryCatch({
          cat(".")
          server <<- startServerFn("0.0.0.0", port, callbacks)
          success <- TRUE
        }, error=function(e) {
          port <<- port + 1L
        })
      }
      invisible(NULL)
    },
    show=function() {
      cat(sprintf("<EpivizServer> port: %d, %s", port, ifelse(socketConnected,"connected","not connected")),"\n")
      invisible(NULL)
    },
    makeCallbacks=function() {
      wsHandler <- .createWSHandler(.self)
      httpHandler <- .createHttpHandler(.self)
      wrapHandlers(wsHandler, httpHandler)      
    },
    startServer=function(...) {
      'start the websocket server'
      callbacks <- makeCallbacks()
      tryCatch({
        server <<- startServerFn("0.0.0.0", port, callbacks)  
      }, error=function(e) {
        if (!tryPorts)
          stop(sprintf("Error starting epivizServer, likely because port %d is in use.\nTry a different port number or setting tryPorts=TRUE (see ?startEpiviz).",port))
        tryMorePorts(callbacks)
      })
      invisible()
    },
    stopServer=function() {
      interrupted <<- TRUE
      
      if (!isClosed()) {
        stopServerFn(server)
      }
      server <<- NULL
      socketConnected <<- FALSE
      interrupted <<- TRUE
      invisible()
    },
    service=function(nonInteractive=FALSE) {
      if (isClosed()) {
        stop("Can't listen, socket is closed")
      }

      if (daemonized)
        return(invisible(TRUE))
      
      if (nonInteractive) {
        # run service loop once
        httpuv::service()
        return(invisible(TRUE))
      }


      interrupted <<- FALSE
      while(!interrupted) {
        httpuv::service()
        Sys.sleep(0.001)
      }
      invisible(TRUE)
    },
    stopService=function() {
      interrupted <<- TRUE
      invisible()
    },
    runServer=function(...) {
      startServer(...)
      on.exit(stopServer())
      service()
    },
    isClosed=function() {
      is.null(server)
    },
    bindManager=function(mgr) {
      msgCallback <<- function(binary, msg) {
        if (binary) {
          msg <- rawToChar(msg)
        }
        
        if (verbose) {
          epivizrMsg("RCVD: ", msg)
        }
        msg = rjson::fromJSON(msg)
        if (msg$type == "request") {
          out=list(type="response",
            requestId=msg$requestId)
          msgData=msg$data
          action=msgData$action
          # request handling
# defined here: http://epiviz.github.io/dataprovider-plugins.html

          out$data=switch(action,
             getMeasurements=mgr$getMeasurements(),
             getRows=mgr$getRows(msgData$seqName,
               msgData$start,
               msgData$end,
               msgData$metadata,
               msgData$datasource),
             getValues=mgr$getValues(msgData$seqName,
               msgData$start,
               msgData$end,
               msgData$datasource,
               msgData$measurement),
             getSeqInfos=mgr$getSeqInfos(),
             getAllData=list(msg=msgData$chr))
          response=rjson::toJSON(out)
          if (verbose) {
            epivizrMsg("SEND: ", response)
          }
          websocket$send(response)
        } else if (msg$type == "response") {
          # TODO: check response success
          callback = mgr$callbackArray$get(msg$requestId)
          if (!is.null(callback)) {
            callback(msg$data)
          }
          popRequest()
        }
      }
      invisible()
    },
    sendRequest=function(request) {
      requestQueue$push(request)
      if (!requestWaiting)
        popRequest()
      invisible()
    },
    popRequest=function() {
      if (!socketConnected) {
        return(invisible())
      }
      request <- requestQueue$pop()
      if (is.null(request)) {
        requestWaiting <<- FALSE
        stopService()
        return(invisible())
      }
      request <- rjson::toJSON(request)
      if (verbose) epivizrMsg("SEND: ", request)
      websocket$send(request)
      requestWaiting <<- TRUE
      service()
    },
    emptyRequestQueue=function() {
      requestQueue$empty()
      inivisible()
    }
  )
)
