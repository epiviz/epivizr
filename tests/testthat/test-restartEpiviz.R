context("restart epiviz")

test_that("restartEpiviz restarts connection and workspace", {
  # TODO(briangottfried): Because EpivizApp$save() checks
  #                       if EpivizApp's server is closed,
  #                       I would like to explore a better
  #                       way to initially call startEpiviz
  #                       in order to save, and restart.
  app <- startEpiviz(non_interactive=TRUE)
  app$server$start_server()
  
  file_name <- "test-restartEpiviz.rda"
  
  expect_false(app$server$is_closed())
  expect_false(file.exists(file_name))
  
  app$save(file=file_name)
  
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

