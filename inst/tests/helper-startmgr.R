.startMGR=function(openBrowser=FALSE, verbose=TRUE, ...) {
  if (openBrowser) {
    tryCatch(startEpiviz(localURL=getOption("epivizrTestURL"), 
    					debug=getOption("epivizrTestDebug"), 
    					proxy=getOption("epivizrTestProxy"),
                                        daemonized=getOption("epivizrTestDaemonized"),
                                        port=getOption("epivizrTestPort"),
    					openBrowser=TRUE, 
    					verbose=verbose, ...), interrupt=function(e) invisible())
  } else {
    startEpiviz(localURL=getOption("epivizrTestURL"),
    			debug=getOption("epivizrTestDebug"),
    			proxy=getOption("epivizrTestProxy"),
                        daemonized=getOption("epivizrTestDaemonized"),
                        port=getOption("epivizrTestPort"),
    			openBrowser=FALSE, 
    			verbose=verbose, ...)
  }
}

wait_until <- function(condition, timeout=3) {
  condition <- substitute(condition)
  ptm <- proc.time()
  while (!eval(condition, parent.frame()) && (proc.time() - ptm < timeout)["elapsed"]) {
    Sys.sleep(0.001)
  }
  expect_true(eval(condition, parent.frame()))
}



