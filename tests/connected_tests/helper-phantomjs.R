.canPhantomTest <- function() {
  if(Sys.which("phantomjs") == "") { return(FALSE) }
  if(!getOption("epivizrCanDaemonize")) { return(FALSE) }
  TRUE
}

remDr <- NULL
pJS <- NULL

.startRemoteDriver <- function() {
  if(!require(RSelenium)) {
    stop("can't run this test here")
  }
  
  if(!.canPhantomTest()) {
    stop("can't do headless testing here")
  }
  
  pJS <<- phantom()
  Sys.sleep(2)
  
  remDr <<- remoteDriver(browserName = 'phantomjs')
  res <- remDr$open()
  invisible()
}

.navigateRemoteDriver <- function(port=7123L, path="", host="127.0.0.1") {
  url <- sprintf("http://%s:%d/%s", host, port, path)
  parallel::mcparallel(remDr$navigate(url), detached=TRUE)
  Sys.sleep(2)
}

.stopPhantomJS <- function() {
  pJS$stop()
}
