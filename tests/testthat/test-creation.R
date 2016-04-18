context("object creation")

test_that("EpivizChartMgr creates a proper object", {
  server <- epivizrServer::createServer()
  mgr <- EpivizChartMgr$new(server)
  
  expect_is(mgr, "EpivizChartMgr")
  
  expect_is(mgr$.chart_list, "environment")
  expect_equal(mgr$num_charts(), 0)
  expect_equal(mgr$.chart_id_counter, 0)
  expect_equal(length(mgr$.chart_type_map), 0)
  
  expect_is(mgr$.server, "EpivizServer")
  expect_true(mgr$is_server_closed())
})

test_that("server opening works as expected", {
  server <- epivizrServer::createServer()
  mgr <- EpivizChartMgr$new(server)
  expect_true(mgr$is_server_closed())
  
  server$start_server()
  on.exit(server$stop_server())
  expect_false(mgr$is_server_closed())
})

test_that("register_chartType works as expected", {
  server <- epivizrServer::createServer()
  mgr <- EpivizChartMgr$new(server)
  mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")  
  expect_equal(length(mgr$.chart_type_map), 1)
  expect_equal(mgr$.chart_type_map$BlockChart, "epiviz.plugins.charts.BlocksTrack")
})

