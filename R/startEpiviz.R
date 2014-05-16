startEpiviz <- function(port=7312L, localURL=NULL, useDevel=FALSE, 
                        chr="chr11", start=99800000, end=103383180, 
                        debug=FALSE, proxy=TRUE, workspace=NULL, scripts=NULL, gists=NULL,
                        openBrowser=TRUE, daemonized=.epivizrCanDaemonize(),
                        verbose=FALSE, nonInteractive=FALSE, tryPorts=FALSE) {

  if (verbose) {
    epivizrMsg("Starting Epivizr!")
  }

  if (missing(localURL) || is.null(localURL)) {
    url <- ifelse(useDevel,"epiviz-dev", "epiviz")
    url <- sprintf("http://%s.cbcb.umd.edu/index.php", url)
  } else {
    url <- localURL
  }
  
  wsURL <- "ws://localhost"
  controllerHost <- sprintf("%s:%d", wsURL, port)

  if (!missing(proxy)) {
    warning("Parameter 'proxy' is no longer used and will be removed in future versions.")
  }
  
  url <- sprintf("%s?websocket-host[]=%s&debug=%s&", 
              url,
              controllerHost,
              ifelse(debug,"true","false"))
  
  if (!is.null(workspace)) {
    url <- paste0(url,"ws=",workspace,"&")
  } else {
    url <- paste0(url,
               sprintf("seqName=%s&start=%d&end=%d&",
                       chr,
                       as.integer(start),
                       as.integer(end)))
  }

  if (!is.null(scripts)) {
    scriptString = paste(sprintf("script[]=%s&", scripts),collapse="")
    url <- paste0(url,scriptString)
  }

  if (!is.null(gists)) {
    gistString <- paste(sprintf("gist[]=%s&", gists), collapse="")
    url <- paste0(url,gistString)
  }

  if (daemonized && !.epivizrCanDaemonize()) {
        warning("You've requested to run non-blocking epivizr, but your version of httpuv does not support it.\n",
                "You can download an appropriate version of httpuv from github:\n",
                "require(devtools); install_github('httpuv', username='epiviz')",call.=FALSE)
        daemonized <- FALSE
  }

  server <- EpivizServer$new(port=port, tryPorts=tryPorts,
                           daemonized=daemonized,verbose=verbose)
  
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
