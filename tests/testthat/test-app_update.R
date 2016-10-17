context("disconnected app data update")

test_that("disconnected data update through app works", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  app <- EpivizApp$new(server=server,
                       data_mgr=data_mgr,
                       chart_mgr=chart_mgr)
  
  gr <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=1))
  gr2 <- GenomicRanges::GRanges(seqnames="chr12", ranges=IRanges::IRanges(start=1:1000,width=10))
  
  chart_mgr$register_chart_type("BlocksTrack")
  chart_obj <- app$plot(gr, datasource_name="ms1", send_request=FALSE)
  chart_id <- chart_obj$get_id()
  
  ms_obj <- app$get_ms_object(chart_obj)
  
  app$update_measurements(ms_obj, gr2)

  ms_obj2 <- app$get_ms_object(chart_obj)
  expect_identical(as(ms_obj2$.object, "GRanges"), gr2)
})