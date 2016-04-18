.constructURL <- function(port=7312L, localURL=NULL, useDevel=FALSE, standalone=FALSE,
                          chr="chr11", start=99800000, end=103383180,
                          debug=FALSE, workspace=NULL, scripts=NULL, gists=NULL, useCookie=FALSE)
  {
  if (missing(localURL) || is.null(localURL)) {
    url <- ifelse(useDevel,"epiviz-dev", "epiviz")
    url <- sprintf("http://%s.cbcb.umd.edu/index.php", url)
  } else {
    url <- localURL
  }

  if (isTRUE(standalone)) {
    url <- sprintf("http://localhost:%d/index-standalone.html?websocket=true", port)
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
    
    cookieString = sprintf("useCookie=%s&", ifelse(useCookie, "true", "false"))
    url <- paste0(url, cookieString)
  }
  
  url
  }

startEpiviz <- function(port=7312L, localURL=NULL, useDevel=FALSE, standalone=FALSE, staticSitePath = "", 
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
                           daemonized=daemonized, standalone=standalone, staticSitePath = staticSitePath, verbose=verbose)

  url <- .constructURL(port=server$port,
                       localURL=localURL,
                       useDevel=useDevel,
                       standalone=standalone,
                       chr=chr,
                       start=start,
                       end=end,
                       debug=debug,
                       workspace=workspace,
                       scripts=scripts,
                       gists=gists)
  
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
