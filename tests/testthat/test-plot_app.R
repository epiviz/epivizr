context("app plot methods")

test_that("plot block works", {
  skip("gc error")
  
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  app <- EpivizApp$new(server=server,
                       data_mgr=data_mgr,
                       chart_mgr=chart_mgr)
  
  gr <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=1))
  chart_mgr$register_chart_type("BlocksTrack", "epiviz.plugins.charts.BlocksTrack")
  chart_obj <- app$plot(gr, datasource_name="ms1", send_request=FALSE)
  chart_id <- chart_obj$get_id()

  ms_obj <- app$get_ms_object(chart_obj)
  ms_id <- ms_obj$get_id()
  ms <- ms_obj$get_measurements()
  
  expect_is(chart_obj, "EpivizChart")
  expect_equal(chart_obj$.measurements, ms)
  expect_equal(chart_obj$.type, "epiviz.plugins.charts.BlocksTrack")
  expect_true(exists(chart_id, env=chart_mgr$.chart_list))
  expect_equal(chart_obj$get_app_id(), character())
})

test_that("plot bp works", {
  skip("gc error")
  
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  app <- EpivizApp$new(server=server,
                       data_mgr=data_mgr,
                       chart_mgr=chart_mgr)
  
  gr <- GenomicRanges::GRanges(seqnames="chr1", 
                               ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), 
                               score1=rnorm(length(seq(1,100,by=25))),
                               score2=rnorm(length(seq(1,100,by=25))))

  chart_mgr$register_chart_type("LineTrack", "epiviz.plugins.charts.LineTrack")
  chart_obj <- app$plot(gr, datasource_name="ms1", type="bp", send_request=FALSE)  
  chart_id <- chart_obj$get_id()

  ms_obj <- app$get_ms_object(chart_obj)
  ms_id <- ms_obj$get_id()
  ms <- ms_obj$get_measurements()
  
  expect_is(chart_obj, "EpivizChart")
  expect_equal(chart_obj$.measurements, ms)
  expect_equal(chart_obj$.type, "epiviz.plugins.charts.LineTrack")
  expect_true(exists(chart_id, env=chart_mgr$.chart_list))
  expect_equal(chart_obj$get_app_id(), character())
})

test_that("plot feature works", {
  skip("gc error")
  
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  
  app <- EpivizApp$new(server=server,
                       data_mgr=data_mgr,
                       chart_mgr=chart_mgr)
  
  
  sset <- make_test_SE()

  chart_mgr$register_chart_type("ScatterPlot", "epiviz.plugins.charts.ScatterPlot")    
  chart_obj <- app$plot(sset, "ms1", send_request=FALSE, columns=c("A","B"), assay="counts2")
  chart_id <- chart_obj$get_id()
  
  ms_obj <- app$get_ms_object(chart_id) 
  ms_id <- ms_obj$get_id()
  ms <- ms_obj$get_measurements()
  
  expect_is(chart_obj, "EpivizChart")
  expect_equal(chart_obj$.measurements, ms)
  expect_equal(chart_obj$.type, "epiviz.plugins.charts.ScatterPlot")
  expect_true(exists(chart_id, env=chart_mgr$.chart_list))
})

test_that("plot gene track works", {
  skip("gc error")
  
  skip_if_not_installed("bumphunter")
  skip_on_cran()
  
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)

  app <- EpivizApp$new(server=server,
                       data_mgr=data_mgr,
                       chart_mgr=chart_mgr)
  
  gr <- make_test_gene_info()

  chart_mgr$register_chart_type("GenesTrack", "epiviz.plugins.charts.GenesTrack")    
  chart_obj <- app$plot(gr, "ms1", type="gene_info", send_request=FALSE)
  chart_id <- chart_obj$get_id()
  
  ms_obj <- app$get_ms_object(chart_id)
  ms_id <- ms_obj$get_id()
  ms <- ms_obj$get_measurements()
  
  expect_is(chart_obj, "EpivizChart")
  expect_equal(chart_obj$.measurements, ms)
  expect_equal(chart_obj$.type, "epiviz.plugins.charts.GenesTrack")
  expect_true(exists(chart_id, env=chart_mgr$.chart_list))
})
