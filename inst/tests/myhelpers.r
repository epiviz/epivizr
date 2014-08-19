setEpivizrTestOpts <- function(sendRequest=TRUE,
                               daemonized=TRUE,
                               local=FALSE,
                               standalone=FALSE,
                               devel=FALSE,
                               test=FALSE,
                               debug=TRUE,
                               port=7312L) {
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

  options(epivizrTestSendRequest=sendRequest,
          epivizrTestDaemonized=daemonized,
          epivizrTestStandalone=standalone,
          epivizrTestURL=url,
          epivizrTestDebug=debug,
          epivizrTestPort=port)
  invisible()
}

getEpivizrTestOpts=function() {
  out <- list(sendRequest=getOption("epivizrTestSendRequest"),
              daemonized=getOption("epivizrTestDaemonized"),
              standalone=getOption("epivizrTestStandalone"),
              url=getOption("epivizrTestURL"),
              debug=getOption("epivizrTestDebug"),
              port=getOption("epivizrTestPort"))
  print(out)
}

setEpivizrTestOpts()

test_srv=function(dem=TRUE, stand=FALSE) {
  setEpivizrTestOpts(daemonized=dem, standalone=stand);
  test(filter=".*server.*")
}

test_reg=function() test(filter=".*register.*")
test_mes=function(req=TRUE,  dem=TRUE,stand=FALSE)
{
  setEpivizrTestOpts(sendRequest=req, daemonized=dem, standalone=stand); test(filter=".*Measure.*")
}
test_fet=function(req=TRUE) {setEpivizrTestOpts(sendRequest=req); test(filter=".*fetch.*")}
test_cha=function(req=TRUE,dem=TRUE,stand=FALSE)
{
  setEpivizrTestOpts(sendRequest=req, daemonized=dem, standalone=stand); test(filter=".*Charts.*")
}
test_dev=function(req=TRUE,dem=TRUE,stand=FALSE)
{
  setEpivizrTestOpts(sendRequest=req, daemonized=dem, standalone=stand); test(filter=".*Device.*")
}

test_some=function(req=TRUE,dem=TRUE,stand=FALSE)
{
  test_mes(req=req,dem=dem,stand=stand);
  test_cha(req=req,dem=dem,stand=stand);
  test_dev(req=req,dem=dem,stand=stand)
}

testb=function() {test_srv(FALSE);test_reg();test_fet(FALSE)}
testb1=function() {test_srv(TRUE); test_reg(); test_fet(TRUE)}
testb2=function() {test_srv(TRUE,TRUE); test_reg(); test_fet(TRUE)}
test0=function() test_some(FALSE,FALSE,FALSE)
test1=function() test_some(TRUE,FALSE,FALSE)
test2=function() test_some(TRUE,TRUE,FALSE)
test3=function() test_some(TRUE,TRUE,TRUE)

test_all=function() {testb(); testb1(); test0(); test1(); test2(); test3()}

