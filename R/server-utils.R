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

.createWSHandler <- function(server) {
  function(ws) {
    if (server$verbose) epivizrMsg("WS opened")
    server$websocket <<- ws
    server$socketConnected <<- TRUE
    server$websocket$onMessage(server$msgCallback)
    server$websocket$onClose(function() {
      server$socketConnected <<- FALSE
      invisible()
    })
    server$popRequest()
    invisible()
  }
}

.createHttpHandler <- function(server) {
  if (server$standalone) {
    filePath <- system.file("inst/www/index-standalone.html", package="epivizr")
    content <- readBin(filePath, 'raw', file.info(filePath)$size)
    
    function(req) {
      list(
        status = 200L,
        headers = list(
          'Content-type' = 'text/html'
          ),
        body=content
        )
    }
  }
  else {
    .dummyTestPage
  }
}

wrapHandlers <- function(wsHandler, httpHandler) {
  list(
    onHeaders = function(req) {
      return(NULL)
    },
    call=httpHandler,
    onWSOpen=wsHandler
  )
}
