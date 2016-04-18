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

#' Start epiviz app and create \code{\class{EpivizApp}} object to manage connection
#' 
#' @param useDevel (logical) use the devel epiviz application server (http://epiviz-dev.cbcb.umd.edu).
#' @param chr (character) chromosome to browse to on app startup.
#' @param start (integer) start location to browse to on app startup.
#' @param end (integer) end location to browse to on app startup.
#' @param debug (logical) start epiviz app in debug mode.
#' @param workspace (character) a workspace id to load in the epiviz app on startup.
#' @param scripts (character) URLs for JavaScript plugin scripts to be imported when epiviz is loaded (see \url{http://epiviz.cbcb.umd.edu/help} for details).
#' @param gists (character) Ids for github gists (\url{http://gist.github.com}) containing JavaScript plugin scripts to
#'  be imported when epiviz is loaded (see \url{http://epiviz.cbcb.umd.edu/help} for details).
#' @param openBrowser (logical) browse to the epiviz URL before exiting function.
#' @param nonInteractive <logical> run in non-interactive mode, for development purposes only.
#' @param ... additional parameters passed to \code{\link[epivizrServer]{createServer}}.
#' 
#' @return An object of class \code{\link{EpivizApp}}
#' @export
startEpiviz <- function(useDevel=FALSE, chr="chr11", start=99800000, end=103383180, 
                        debug=FALSE, workspace=NULL, scripts=NULL, gists=NULL,
                        openBrowser=TRUE, nonInteractive=FALSE, ...) {

  server <- epivizrServer::createServer(...)
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  app <- EpivizApp$new(server=server,
                       data_mgr=data_mgr,
                       chart_mgr=chart_mgr)
  return(app)
  
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
