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
#' @param port <integer> The port to open server to
#' @param localURL <character> URL to open
#' @param useDevel <logical> Use the epiviz-dev app
#' @param standalone <logical> Is it running standalone through R
#' @param staticSitePath <character> Directory to serve standalone application
#' @param chr <character> Chromosome to load at epiviz app start
#' @param start <integer> Starting location to load at epiviz app start
#' @param end <integer> Ending location to load at epiviz app start
#' @param debug <logical> Use debug user for epiviz app workspace storage
#' @param workspace <character> Id of workspace to load on epiviz app start
#' @param scripts <character> Path to JS scripts to include on epiviz app start
#' @param gists <character> Vector of github gist ids with JS code to include on epiviz app start
#' @param openBrowser <logical> Open browser at URL through this function call
#' @param daemonized <logical> Run app in non-blocking mode
#' @param verbose <logical> Print verbose information
#' @param nonInteractive <logical> Only used internally
#' @param tryPorts <logical> Try multiple ports until finding an open port when opening WebSocket connection
#' 
#' @return An object of class \code{\link{EpivizApp}}
#' @export
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
