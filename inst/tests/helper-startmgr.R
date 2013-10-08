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

sendRequest = FALSE

