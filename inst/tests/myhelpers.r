makeEpivizrTestOpts <- function(sendRequest=FALSE,
                                daemonized=FALSE,
                                url="http://localhost/~hcorrada/epiviz/test_socket.php",
                                debug=TRUE,
                                proxy=TRUE) {

  list(sendRequest=sendRequest, daemonized=daemonized, url=url, debug=debug, proxy=proxy)
}

.epivizrTestOpts <<- makeEpivizrTestOpts()

test_srv=function() test(filter=".*server.*")
test_reg=function() test(filter=".*register.*")
test_mes=function(req=TRUE,dem=TRUE) {.epivizrTestOpts <<- makeEpivizrTestOpts(sendRequest=req, daemonized=dem); test(filter=".*Measure.*")}
test_cha=function(req=TRUE,dem=TRUE) {.epivizrTestOpts <<- makeEpivizrTestOpts(sendRequest=req, daemonized=dem); test(filter=".*Charts.*")}
test_dev=function(req=TRUE,dem=TRUE) {.epivizrTestOpts <<- makeEpivizrTestOpts(sendRequest=req, daemonized=dem); test(filter=".*Device.*")}

test_some=function(req=TRUE,dem=TRUE) {test_mes(req=req,dem=dem); test_cha(req=req,dem=dem); test_dev(req=req,dem=dem)}

testb=function() {test_srv(); test_reg()}
test0=function() test_some(FALSE,FALSE)
test1=function() test_some(TRUE,FALSE)
test2=function() test_some(TRUE,TRUE)

test_all=function() {testb(); test0(); test1(); test2()}

