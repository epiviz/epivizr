startEpiviz <- function(port=7312L, localURL=NULL, useDevel=FALSE, 
                        chr="chr11", start=99800000, end=103383180, 
                        debug=FALSE, proxy=TRUE, workspace=NULL, 
                        openBrowser=TRUE, daemonized=TRUE,
                        verbose=FALSE, nonInteractive=FALSE, tryPorts=FALSE) {

  if (verbose) {
    epivizrMsg("Starting Epivizr!")
  }
  server <- EpivizServer$new(port=port, tryPorts=tryPorts, daemonized=daemonized,verbose=verbose)
  
  if (missing(localURL) || is.null(localURL)) {
    url <- ifelse(useDevel,"epiviz-dev", "epiviz")
    url <- sprintf("http://%s.cbcb.umd.edu/index.php", url)
  } else {
    url <- localURL
  }
  
  wsURL <- "ws://localhost"
  controllerHost <- sprintf("%s:%d", wsURL, port)
  
  url <- sprintf("%s?controllerHost=%s&debug=%s&proxy=%s&", 
              url,
              controllerHost,
              ifelse(debug,"true","false"),
              ifelse(proxy,"true","false"))
  
  if (!is.null(workspace)) {
    url <- paste0(url,"workspace=",workspace,"&")
  } else {
    url <- paste0(url,
               sprintf("chr=%s&start=%d&end=%d&",
                       chr,
                       as.integer(start),
                       as.integer(end)))
  }

  if (verbose) {
    epivizrMsg("Initializing session manager...")
  }
  tryCatch({
    mgr <- EpivizDeviceMgr$new(server=server, url=url, verbose=verbose, nonInteractive=nonInteractive)
    mgr$bindToServer()
  }, error=function(e) {
    server$stopServer()
    stop("Error starting Epiviz: ", e)
  })
  
  if (verbose) {
    epivizrMsg("Opening connections...")
  }

  if (openBrowser) {
    tryCatch({
      mgr$openBrowser(url)
    }, error=function(e) {
      mgr$stopServer()
      stop("Error starting Epiviz: ", e)
    }, interrupt=function(e) {NULL})
  }

  if (verbose) {
    epivizrMsg("Done starting Epivizr!")
  }
  return(mgr)
}
