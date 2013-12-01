localURL="http://localhost/~hcorrada/epiviz/test_socket.php"
#localURL="http://epiviz.cbcb.umd.edu/test_socket.php"
#localURL=NULL
debug=TRUE
proxy=TRUE
.startMGR=function(openBrowser=FALSE,...) {
  if (openBrowser) {
    tryCatch(startEpiviz(localURL=localURL, 
    					debug=debug, 
    					proxy=proxy, 
    					openBrowser=TRUE, 
    					verbose=TRUE, ...), interrupt=function(e) invisible())
  } else {
    startEpiviz(localURL=localURL,
    			debug=debug,
    			proxy=proxy,
    			openBrowser=FALSE, 
    			verbose=TRUE, ...)
  }
}

sendRequest = TRUE

ensureConnection <- function(mgr, timeout=30) {
       ptm <- proc.time()
       while(!mgr$server$socketConnected && (proc.time()-ptm)[2] * 1000 <= 30) {
         if (!mgr$server$daemonized) {
           service(verbose=FALSE)
         } else {
           Sys.sleep(0.01)
         }
       }
       mgr$server$socketConnected
}

pauseForInterrupt <- function(doIt=TRUE) {
  if (!doIt)
    return(invisible(NULL))
  
  tryCatch({
    while(TRUE) {
      Sys.sleep(0.001)
    }
  }, interrupt=function(int) invisible())
}
