if (is.null(getOption("epivizrTestSendRequest")))
  options(epivizrTestSendRequest=TRUE)

if (is.null(getOption("epivizrTestDaemonized")))
  options(epivizrTestDaemonized=TRUE)

if (is.null(getOption("epivizrTestURL")))
  options(epivizrTestURL="http://epiviz.cbcb.umd.edu/test_socket.php")

if (is.null(getOption("epivizrTestDebug")))
  options(epivizrTestDebug=TRUE)

if (is.null(getOption("epivizrTestProxy")))
  options(epivizrTestProxy=TRUE)

options(epivizrTesting=TRUE)
