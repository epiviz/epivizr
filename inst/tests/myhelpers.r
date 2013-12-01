setEpivizrTestOpts <- function(sendRequest=TRUE,
                                daemonized=TRUE,
                               local=FALSE,
                                devel=FALSE,
                               test=TRUE,
                                debug=TRUE,
                                proxy=TRUE) {
  url <- if (devel) "epiviz-dev" else "epiviz"

  if (!local) {
    url <- sprintf("http://%s.cbcb.umd.edu", url)
  } else {
    url <- sprintf("http://localhost/~hcorrada/%s", url)
  }
  
  if (test) {
    url <- paste(url, "test_socket.php", sep="/")
  } else {
    url <- paste(url, "index.php", sep="/")
  }

  .epivizrTestOpts <<- list(sendRequest=sendRequest, daemonized=daemonized, url=url, debug=debug, proxy=proxy)
  invisible()
}

setEpivizrTestOpts()

test_srv=function() test(filter=".*server.*")
test_reg=function() test(filter=".*register.*")
test_mes=function(req=TRUE,dem=TRUE) {setEpivizrTestOpts(sendRequest=req, daemonized=dem); test(filter=".*Measure.*")}
test_cha=function(req=TRUE,dem=TRUE) {setEpivizrTestOpts(sendRequest=req, daemonized=dem); test(filter=".*Charts.*")}
test_dev=function(req=TRUE,dem=TRUE) {setEpivizrTestOpts(sendRequest=req, daemonized=dem); test(filter=".*Device.*")}

test_some=function(req=TRUE,dem=TRUE) {test_mes(req=req,dem=dem); test_cha(req=req,dem=dem); test_dev(req=req,dem=dem)}

testb=function() {test_srv(); test_reg()}
test0=function() test_some(FALSE,FALSE)
test1=function() test_some(TRUE,FALSE)
test2=function() test_some(TRUE,TRUE)

test_all=function() {testb(); test0(); test1(); test2()}

