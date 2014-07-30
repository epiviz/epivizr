.constructURL <- function(port=7312L, localURL=NULL, useDevel=FALSE, standalone=FALSE,
                          chr="chr11", start=99800000, end=103383180,
                          debug=FALSE, workspace=NULL, scripts=NULL, gists=NULL)
  {
  if (missing(localURL) || is.null(localURL)) {
    url <- ifelse(useDevel,"epiviz-dev", "epiviz")
    url <- sprintf("http://%s.cbcb.umd.edu/index.php", url)
  } else {
    url <- localURL
  }

  if (isTRUE(standalone)) {
    url <- sprintf("http://localhost:%d/index-standalone.html", port)
  }

  if (!isTRUE(standalone)) {
    controllerHost <- sprintf("ws://localhost:%d", port)  
    url <- sprintf("%s?websocket-host[]=%s&", url, controllerHost)

    url <- paste0(url, sprintf("debug=%s&", ifelse(debug, "true", "false")))
  
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
  }
  
  url
  }

startEpiviz <- function(port=7312L, localURL=NULL, useDevel=FALSE, standalone=FALSE, 
                        chr="chr11", start=99800000, end=103383180, 
                        debug=FALSE, workspace=NULL, scripts=NULL, gists=NULL,
                        openBrowser=TRUE, daemonized=.epivizrCanDaemonize(),
                        verbose=FALSE, nonInteractive=FALSE, tryPorts=FALSE) {

  if (verbose) {
    epivizrMsg("Starting Epivizr!")
  }
  
  if (daemonized && !.epivizrCanDaemonize()) {
        warning("You've requested to run non-blocking epivizr, but your version of httpuv does not support it.\n",
                "You can download an appropriate version of httpuv from github:\n",
                "require(devtools); install_github('httpuv', username='epiviz')",call.=FALSE)
        daemonized <- FALSE
  }

  server <- EpivizServer$new(port=port, tryPorts=tryPorts,
                           daemonized=daemonized,verbose=verbose)

  # change port if server started on a different port
  if (server$port != port && tryPorts) 
    port <- server$port
  
  url <- .constructURL(port, localURL, useDevel, standalone, chr, start, end, debug, workspace, scripts, gists)
  
  if (verbose) {
    epivizrMsg("Initializing session manager...")
  }
  tryCatch({
    mgr <- EpivizDeviceMgr$new(server=server, url=url, verbose=verbose, nonInteractive=nonInteractive)
    mgr$bindToServer()

    if (verbose) {
      epivizrMsg("Opening connections...")
    }
    
    if (openBrowser) {
      mgr$openBrowser(url)
    }

    if (verbose) {
      epivizrMsg("Done starting Epivizr!")
    }
    return(mgr)
  }, error=function(e) {
    server$stopServer()
    stop("Error starting Epiviz: ", e)
  }, interrrupt=function(e) {NULL})  
}
