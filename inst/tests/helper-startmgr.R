.startMGR=function(openBrowser=FALSE, testing=TRUE, verbose=TRUE, ...) {
  if (openBrowser) {
    tryCatch(startEpiviz(localURL=.epivizrTestOpts$url, 
    					debug=.epivizrTestOpts$debug, 
    					proxy=.epivizrTestOpts$proxy,
                                        daemonized=.epivizrTestOpts$daemonized,
    					openBrowser=TRUE, 
    					verbose=verbose, testing=testing, ...), interrupt=function(e) invisible())
  } else {
    startEpiviz(localURL=.epivizrTestOpts$url,
    			debug=.epivizrTestOpts$debug,
    			proxy=.epivizrTestOpts$proxy,
                        daemonized=.epivizrTestOpts$daemonized,
    			openBrowser=FALSE, 
    			verbose=verbose, testing=testing, ...)
  }
}

wait_until <- function(condition, timeout=3) {
  ptm <- proc.time()
  while (!eval(condition) && (proc.time() - ptm < timeout)["elapsed"]) {
    Sys.sleep(0.001)
  }
  expect_true(eval(condition))
}



