context("browser commands")

test_that("navigate works", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  
  app <- EpivizApp$new(server=server,
                       data_mgr=data_mgr,
                       chart_mgr=chart_mgr)
  app$navigate(chr="chr10", start=2000, end=10000)
})

test_that("slideshow works", {
  server <- epivizrServer::createServer(non_interactive=TRUE)
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  
  app <- EpivizApp$new(server=server,
    data_mgr=data_mgr,
    chart_mgr=chart_mgr)
  
  granges <- GenomicRanges::GRanges("chr10", 
    IRanges::IRanges(start=(1:10)*1000, width=10000))
  expect_error(app$slideshow(granges, n=4))
})