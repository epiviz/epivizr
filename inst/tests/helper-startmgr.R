.startMGR=function(openBrowser=FALSE,...) {
  if (openBrowser) {
    tryCatch(startEpiviz(localURL=epivizrTestOpts$url, 
    					debug=epivizrTestOpts$debug, 
    					proxy=epivizrTestOpts$proxy, 
    					openBrowser=TRUE, 
    					verbose=TRUE, testing=TRUE, ...), interrupt=function(e) invisible())
  } else {
    startEpiviz(localURL=epivizrTestOpts$url,
    			debug=epivizrTestOpts$debug,
    			proxy=epivizrTestOpts$proxy,
    			openBrowser=FALSE, 
    			verbose=TRUE, testing=TRUE, ...)
  }
}

wait_until <- function(condition, timeout=3) {
  ptm <- proc.time()
  while (!eval(condition) && (proc.time() - ptm < timeout)["elapsed"]) {
    Sys.sleep(0.001)
  }
  expect_true(eval(condition))
}



