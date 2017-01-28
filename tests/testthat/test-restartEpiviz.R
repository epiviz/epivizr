context("restart epiviz")

test_that("restartEpiviz restarts connection and workspace", {
  app <- startEpiviz(open_browser=FALSE,
                     port=7123L,
                     static_site_path=".",
                     daemonized=TRUE,
                     verbose=TRUE)
  app$server$start_server()
  
  file_name <- "test-restartEpiviz.rda"
  expect_false(app$server$is_closed())
  expect_false(file.exists(file_name))
  app$save(file_name)
  expect_true(file.exists(file_name))
  expect_true(app$server$is_closed())
  
  app <- restartEpiviz(file_name)
  expect_false(app$server$is_closed())
  expect_is(app, "EpivizApp")
  expect_is(app$server, "EpivizServer")
  expect_is(app$chart_mgr, "EpivizChartMgr")
  expect_is(app$data_mgr, "EpivizDataMgr")
  
  app$stop_app()
  app$server$stop_server()
  file.remove(file_name)
})

