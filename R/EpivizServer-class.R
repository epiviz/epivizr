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
    startServerFn="function",
    stopServerFn="function"
  ),
  methods=list(
    initialize=function(port=7312L, tryPorts=FALSE, daemonized=NULL, verbose=FALSE, ...) {
      port <<- port
      interrupted <<- FALSE
      socketConnected <<- FALSE
      server <<- NULL
      tryPorts <<- tryPorts
      requestWaiting <<- FALSE
      daemonized <<-  .epivizrCanDaemonize() && isTRUE(daemonized)
      startServerFn <<- if (.self$daemonized) httpuv::startDaemonizedServer else httpuv::startServer
      stopServerFn <<- if (.self$daemonized) httpuv::stopDaemonizedServer else httpuv::stopServer
      verbose <<- verbose
      callSuper(...)
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
    startServer=function(...) {
      'start the websocket server'
      callbacks <- list(
        call=.self$.dummyTestPage,
        onWSOpen=function(ws) {
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
      )
      
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
            requestId=msg$requestId,
            data=mgr$processRequest(msg$data))
          response=rjson::toJSON(out)
          websocket$send(response)
        } else if (msg$type == "response") {
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
 
# Requests to the JS web app
#EpivizServer$methods(
#    addMeasurements=function(requestId, msType, measurements) {
#      request=list(type="request",
#                   id=requestId,
#                   action="addMeasurements",
#                   data=list(measurements=measurements,
#                             type=msType))
#      sendRequest(request)
#    },    
#    rmMeasurements=function(requestId, measurements, msType) {
#      request <- list(type="request",
#                      id=requestId,
#                      action="rmMeasurements",
#                      data=list(measurements=measurements,
#                                type=msType))
#      sendRequest(request)
#    },
#    addChart=function(requestId, chartType, measurements) {
#      request=list(type="request",
#                   id=requestId,
#                   action="addChart",
#                   data=list(measurements=measurements,
#                             type=chartType))
#      sendRequest(request)
#    },
#    rmChart=function(requestId, chartId) {
#      request=list(type="request",
#                   id=requestId,
#                   action="rmChart",
#                   data=list(chartId=chartId))
#      sendRequest(request)
#    },
#    clearChartCaches=function(requestId, chartIds) {
#      request=list(type="request",
#                   id=requestId,
#                   action="clearDeviceCaches",
#                   data=list(ids=chartIds))
#      sendRequest(request)
#    },
#    refresh=function() {
#      request=list(action="refresh")
#      # TODO: finish implementation
#      # sendRequest(request)
#      invisible()
#    },
#    navigate=function(requestId,chr,start,end) {
#      request=list(type="request",
#                   action="navigate",
#                   id=requestId,
#                   data=list(chr=chr,start=start,end=end))
#      sendRequest(request)
#    }
#)
#
EpivizServer$methods(
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
          '  var data = JSON.parse(msg.data)',
          '  msgDiv = document.createElement("pre");',
          '  msgDiv.innerHTML = data.data.replace(/&/g, "&amp;").replace(/\\</g, "&lt;");',
          '  document.getElementById("output").appendChild(msgDiv);',
          '  ws.send(JSON.stringify({type: "response", requestId: data.requestId, data: "that msg"}));',
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
)                           
