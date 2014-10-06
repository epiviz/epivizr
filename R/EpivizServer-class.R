.dummyTestPage=function(req) {
  wsUrl = paste(sep='',
    '"',
    "ws://",
    ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
    '"')
  
  list(
    status = 200L,
    headers = list(
      'Content-Type' = 'text/html'
      ),
    body = paste(
      sep = "\r\n",
      "<!DOCTYPE html>",
      "<html>",
      "<head>",
      '<style type="text/css">',
      'body { font-family: Helvetica; }',
      'pre { margin: 0 }',
      '</style>',
      "<script>",
      sprintf("var ws = new WebSocket(%s);", wsUrl),
      "ws.onmessage = function(msg) {",
      '  var req = JSON.parse(msg.data)',
      '  msgDiv = document.createElement("pre");',
      '  msgDiv.innerHTML = req.data.msg.replace(/&/g, "&amp;").replace(/\\</g, "&lt;");',
      '  document.getElementById("output").appendChild(msgDiv);',
      '  ws.send(JSON.stringify({type: "response", requestId: req.requestId, data: {msg: "that msg"}}));',
      "}",
      "function sendInput() {",
      "  var input = document.getElementById('input');",
      "  ws.send(JSON.stringify({type: 'request', requestId: 0, data: {action: 'getAllData', measurements: {}, chr: input.value, start: 0, end: 0}}));",
      "  input.value = '';",
      "}",
      "</script>",
      "</head>",
      "<body>",
      '<h3>Send Message</h3>',
      '<form action="" onsubmit="sendInput(); return false">',
      '<input type="text" id="input"/>',
      '<h3>Received</h3>',
      '<div id="output"/>',
      '</form>',
      "</body>",
      "</html>"
      )
    )
}

.standalonePage <- function(path="") {
    if (path == "") {
        filePath <- system.file("inst","www", package="epivizr")
    } else {
        filePath <- path
    }
    epivizrMsg("loading standalone from ", filePath)
  staticHandler(filePath)
}

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
    staticSitePath="character",
    startServerFn="function",
    stopServerFn="function"
  ),
  methods=list(
    initialize=function(port=7312L, tryPorts=FALSE, daemonized=NULL, standalone=NULL, verbose=FALSE, staticSitePath="", ...) {
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
      staticSitePath <<- staticSitePath 
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
      wsHandler <- function(ws) {
        if (verbose) epivizrMsg("WS opened")
        websocket <<- ws
        socketConnected <<- TRUE
        websocket$onMessage(.self$msgCallback)
        websocket$onClose(function() {
          socketConnected <<- FALSE
          invisible()
        })
        popRequest()
        invisible()
      }
      
      if (standalone) {
        httpHandler <- .standalonePage(staticSitePath)
      } else {
        httpHandler <- .dummyTestPage
      }

      handlerMgr <- HandlerManager$new()
      handlerMgr$addHandler(httpHandler, 'static')
      handlerMgr$addWSHandler(wsHandler, 'ws')
      handlerMgr$createHttpuvApp()
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
