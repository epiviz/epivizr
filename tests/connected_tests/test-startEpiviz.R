context("connected start epiviz")

test_that("startEpiviz creates a proper object", {
  skip_on_os("windows")
  skip_if_not_installed("RSelenium")
  
  if (!.canPhantomTest()) {
    skip("This test can't be run in this environment.")
  }
  
  app <- startEpiviz(open_browser=FALSE,
                     port=7123L,
                     static_site_path=".",
                     daemonized=TRUE,
                     verbose=TRUE)
  
  if (!app$server$is_daemonized()) {
    skip("This test can only run for daemonized servers.")
  }
  
  .startRemoteDriver()
  on.exit(.stopPhantomJS())
  
  app$server$start_server()
  on.exit(app$server$stop_server(), add=TRUE)

  .navigateRemoteDriver(port=app$server$.port)
  wait_until(app$server$is_socket_connected())
  
  title <- remDr$getTitle()[[1]]
  expect_equal(title, "EpivizServer Test Page")
  
  expect_false(app$server$is_closed())
  remDr$close()
})
