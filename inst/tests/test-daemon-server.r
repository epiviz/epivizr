context("daemonized server")

mgr<-list(getData=function(measurements, chr, start, end) {
  return(chr)
},verbose=TRUE)

test_that("constructor creates a proper object", {
  server <- epivizr:::EpivizServer$new(port=7123L, daemonized=TRUE)
  expect_is(server, "EpivizServer")
  expect_true(server$isClosed())
  expect_equal(.Platform$OS.type == "unix", server$daemonized)
})

test_that("startServer and stopServer work appropriately", {
  server <- epivizr:::EpivizServer$new(port=7123L, daemonized=TRUE)
  expect_true(server$isClosed())
  
  server$startServer()
  expect_false(server$isClosed())
  
  server$stopServer()
  expect_true(server$isClosed())
})

test_that("socket messaging works", {
  server <- epivizr:::EpivizServer$new(port=7123L, daemonized=TRUE)
  server$bindManager(mgr)
  server$startServer()
  
  browseURL("http://localhost:7123/")
  tryCatch(while(TRUE) {Sys.sleep(0.01)}, interrupt=function(int) invisible())
  
  expect_false(server$isClosed())
  
  server$stopServer()
  expect_true(server$isClosed())
})
  

test_that("runServer works", {
  server <- epivizr:::EpivizServer$new(port=7123L, daemonized=TRUE)
  server$bindManager(mgr)
  
  browseURL("http://localhost:7123/")
  tryCatch(server$runServer(), interrupt=function(int) invisible())
  
  expect_true(server$isClosed())
})

test_that("new error message is displayed", {
  server <- epivizr:::EpivizServer$new(port=7123L, daemonized=TRUE)
  server$bindManager(mgr)
  server$startServer()
#  tryCatch(server$service(), interrupt=function(int) invisible())
  expect_false(server$isClosed())

  server2 <- epivizr:::EpivizServer$new(port=7123L)
  expect_error(tryCatch(server2$startServer(),error=function(e){print(e); stop(e)}))

  server$stopServer()
  expect_true(server$isClosed())
})

test_that("tryPorts works", {
  server <- epivizr:::EpivizServer$new(port=7123L, daemonized=TRUE)
  server$bindManager(mgr)
  server$startServer()
#  tryCatch(server$service(), interrupt=function(int) invisible())
  expect_false(server$isClosed())

  server2 <- epivizr:::EpivizServer$new(port=7123L, tryPorts=TRUE, daemonized=TRUE)
  server2$bindManager(mgr)
  server2$startServer()
#  tryCatch(server2$service(), interrupt=function(int) invisible())
  expect_false(server2$isClosed())

  browseURL(sprintf("http://localhost:%d/", server2$port))
  tryCatch(while(TRUE) {Sys.sleep(0.001)}, interrupt=function(int) invisible())

  server$stopServer()
  server2$stopServer()
  expect_true(server$isClosed())
  expect_true(server2$isClosed())
})

