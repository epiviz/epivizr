context("server")

constrFunction <- function(...) epivizr:::EpivizServer$new(daemonized=getOption("epivizrTestDaemonized"), ...)

mgr <- new.env()
mgr$lastMessage <- ""
mgr$callbackArray <- epivizr:::IndexedArray$new()
mgr$verbose <- TRUE
mgr$processRequest <- function(data) {
  action <- data$action
  print(action)
  switch(action,
         getAllData=data$chr)
}

mgr$makeRequest <- function(msg) {
                           callback=function(newmsg) {
                             mgr$lastMessage <<- newmsg
                             epivizrMsg("Response received")
                           }
                           requestId <- mgr$callbackArray$append(callback)
                           list(type="request",
                                id=requestId,
                                action="writeMsg",
                                data=msg)
                         }

test_that("constructor creates a proper object", {
  server <- constrFunction(port=7123L)
  expect_is(server, "EpivizServer")
  expect_true(server$isClosed())
  expect_equal(server$daemonized, getOption("epivizrTestDaemonized"))
})

test_that("startServer and stopServer work appropriately", {
  server <- constrFunction(port=7123L)
  expect_true(server$isClosed())
  
  server$startServer()
  expect_false(server$isClosed())
  expect_equal(server$daemonized, getOption("epivizrTestDaemonized"))
  server$stopServer()
  expect_true(server$isClosed())
})

test_that("socket messaging works", {
  server <- constrFunction(port=7123L)
  server$bindManager(mgr)
  server$startServer()
  
  browseURL("http://localhost:7123/")
  tryCatch(server$service(), interrupt=function(int) invisible())
  wait_until(server$socketConnected)
  
  expect_false(server$isClosed())

  request <- mgr$makeRequest("this msg")
  server$sendRequest(request)

  wait_until(!server$requestWaiting)
  server$stopServer()
  expect_true(server$isClosed())
})
  

test_that("new error message is displayed", {
  server <- constrFunction(port=7123L)
  server$bindManager(mgr)
  server$startServer()
#  tryCatch(server$service(), interrupt=function(int) invisible())
 # expect_false(server$isClosed())

  server2 <- constrFunction(port=7123L)
  expect_error(tryCatch(server2$startServer(),error=function(e){print(e); stop(e)}))

  server$stopServer()
  expect_true(server$isClosed())
})

test_that("tryPorts works", {
  server <- constrFunction(port=7123L)
  server$bindManager(mgr)
  server$startServer()

#  browseURL(sprintf("http://localhost:7312/")
 ## tryCatch(server$service(), interrupt=function(int) invisible())
  #wait_until(substitute(server$socketConnected))
  
  expect_false(server$isClosed())

  server2 <- constrFunction(port=7123L, tryPorts=TRUE)
  server2$bindManager(mgr)
  server2$startServer()
  #tryCatch(server2$service(), interrupt=function(int) invisible())
 # wait_until(substitute(server$socketConnected))
  
  expect_false(server2$isClosed())

  browseURL(sprintf("http://localhost:%d/", server2$port))
  tryCatch(server2$service(), interrupt=function(int) invisible())
  wait_until(server2$socketConnected)
  
  server$stopServer()

  server2$sendRequest(mgr$makeRequest("this other msg"))
  server2$stopServer()
  expect_true(server$isClosed())
  expect_true(server2$isClosed())
})

