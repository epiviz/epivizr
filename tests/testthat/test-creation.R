context("object creation")

test_that("EpivizChartMgr creates a proper object", {
  skip("gc error")
  
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
  skip("gc error")
  
  server <- epivizrServer::createServer(try_ports=TRUE)
  mgr <- EpivizChartMgr$new(server)
  expect_true(mgr$is_server_closed())
  
  server$start_server()
  on.exit(server$stop_server())
  expect_false(mgr$is_server_closed())
})

test_that("register_chartType works as expected", {
  skip("gc error")
  
  server <- epivizrServer::createServer()
  mgr <- EpivizChartMgr$new(server)
  mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")  
  expect_equal(length(mgr$.chart_type_map), 1)
  expect_equal(mgr$.chart_type_map$BlockChart, "epiviz.plugins.charts.BlocksTrack")
})

test_that("EpivizApp works as expected", {
  skip("gc error")
  
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  app <- EpivizApp$new(server=server,
                       data_mgr=data_mgr,
                       chart_mgr=chart_mgr)
  
  expect_is(app, "EpivizApp")
  expect_is(app$server, "EpivizServer")

  expect_is(app$chart_mgr, "EpivizChartMgr")
  expect_equal(app$server, app$chart_mgr$.server)
  
  expect_is(app$data_mgr, "EpivizDataMgr")
  expect_equal(app$server, app$data_mgr$.server)
  
  expect_equal(app$data_mgr$num_datasources(), 0)
  expect_equal(app$chart_mgr$num_charts(), 0)
})
