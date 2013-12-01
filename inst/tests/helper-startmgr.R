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

wait_until <- function(condition, timeout=30) {
  ptm <- proc.time()
  while (!eval(condition) && (proc.time() - ptm < timeout)["elapsed"]) {
    Sys.sleep(0.001)
  }
  expect_true(eval(condition))
}



