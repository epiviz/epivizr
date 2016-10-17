context("start epiviz")

test_that("startEpiviz creates a proper object", {
  app <- startEpiviz(non_interactive=TRUE)
  expect_is(app, "EpivizApp")
  
  expect_is(app$server, "EpivizServer")
  expect_is(app$chart_mgr, "EpivizChartMgr")
  expect_is(app$data_mgr, "EpivizDataMgr")
  
  expect_true(app$server$is_closed())
})
