context("disconnected manage charts")

test_that("rm_chart works usign chart object", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  
  gr <- GenomicRanges::GRanges(seqnames="chr1", 
    ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),
    score2=rnorm(length(seq(1,100,by=25))))

  ms_obj <- data_mgr$add_measurements(gr, "ms1", type="bp", send_request=FALSE)
  ms_id <- ms_obj$get_id()
  
  chart_mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")
  chart_obj <- chart_mgr$visualize("BlockChart", datasource=ms_obj)
  chart_id <- chart_obj$get_id()
  
  chart_mgr$rm_chart(chart_obj)
  expect_equal(chart_mgr$num_charts(), 0)
  expect_false(exists(chart_id, env=chart_mgr$.chart_list, inherits=FALSE))
})

test_that("rm_chart works usign chart id", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  
  gr <- GenomicRanges::GRanges(seqnames="chr1", 
                               ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), 
                               score1=rnorm(length(seq(1,100,by=25))),
                               score2=rnorm(length(seq(1,100,by=25))))
  
  ms_obj <- data_mgr$add_measurements(gr, "ms1", type="bp", send_request=FALSE)
  ms_id <- ms_obj$get_id()
  
  chart_mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")
  chart_obj <- chart_mgr$visualize("BlockChart", datasource=ms_obj)
  chart_id <- chart_obj$get_id()
  
  chart_mgr$rm_chart(chart_id)
  
  expect_equal(chart_mgr$num_charts(), 0)
  expect_false(exists(chart_id, env=chart_mgr$.chart_list, inherits=FALSE))
})

test_that("rm_all_charts works", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  
  gr1 <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=100))
  gr2 <- GenomicRanges::GRanges(seqnames="chr2", ranges=IRanges::IRanges(start=2:20, width=100))
  gr3 <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  se <- make_test_SE()
  
  dev1 <- data_mgr$add_measurements(gr1, "dev1", send_request=FALSE); devId1 <- dev1$get_id()
  dev2 <- data_mgr$add_measurements(gr2, "dev2", send_request=FALSE); devId2 <- dev2$get_id()
  dev3 <- data_mgr$add_measurements(gr3, "dev3", send_request=FALSE, type="bp"); devId3 <- dev3$get_id()
  dev4 <- data_mgr$add_measurements(se, "dev4", send_request=send_request, columns=c("A", "B")); devId4 <- dev4$get_id()

  chart_mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")
  chart_mgr$register_chart_type("LineChart", "epiviz.plugins.charts.LineTrack")
  chart_mgr$register_chart_type("ScatterChart", "epiviz.plugins.charts.ScatterChart")
  
  chart1 <- chart_mgr$visualize("BlockChart", datasource=dev1)
  chart2 <- chart_mgr$visualize("BlockChart", datasource=dev2)
  chart3 <- chart_mgr$visualize("LineChart", datasource=dev3)
  chart4 <- chart_mgr$visualize("ScatterChart", datasource=dev4)

  expect_equal(chart_mgr$num_charts(), 4)    
  chart_mgr$rm_all_charts()
  expect_equal(chart_mgr$num_charts(), 0)
})

test_that("list_charts works", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  
  gr1 <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=1:10, width=100))
  gr2 <- GenomicRanges::GRanges(seqnames="chr2", ranges=IRanges::IRanges(start=2:20, width=100))
  gr3 <- GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  se <- make_test_SE()
  
  dev1 <- data_mgr$add_measurements(gr1, "dev1", send_request=FALSE); devId1 <- dev1$get_id()
  dev2 <- data_mgr$add_measurements(gr2, "dev2", send_request=FALSE); devId2 <- dev2$get_id()
  dev3 <- data_mgr$add_measurements(gr3, "dev3", send_request=FALSE, type="bp"); devId3 <- dev3$get_id()
  dev4 <- data_mgr$add_measurements(se, "dev4", send_request=send_request, columns=c("A", "B")); devId4 <- dev4$get_id()
  
  chart_mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")
  chart_mgr$register_chart_type("LineChart", "epiviz.plugins.charts.LineTrack")
  chart_mgr$register_chart_type("ScatterPlot", "epiviz.plugins.charts.ScatterPlot")
  
  chart1 <- chart_mgr$visualize("BlockChart", datasource=dev1)
  chart2 <- chart_mgr$visualize("BlockChart", datasource=dev2)
  chart3 <- chart_mgr$visualize("LineChart", datasource=dev3)
  chart4 <- chart_mgr$visualize("ScatterPlot", datasource=dev4)
  charts_list <- chart_mgr$list_charts()
    
  ids <- c(chart1$get_id(), chart2$get_id(), chart3$get_id(), chart4$get_id())
  type <- c("epiviz.plugins.charts.BlocksTrack",
            "epiviz.plugins.charts.BlocksTrack",
            "epiviz.plugins.charts.LineTrack",
            "epiviz.plugins.charts.ScatterPlot")
    ms <- c(paste0(dev1$get_id(),":",dev1$get_name()),
            paste0(dev2$get_id(),":",dev2$get_name()), 
            paste0(dev3$get_id(), ":score"),
            paste0(dev4$get_id(), ":", c("A","B"), collapse=","))
    connected <- rep("", 4)
    expected_df <- data.frame(id=ids,
                              type=type,
                              measurements=ms,
                              connected=connected,
                              stringsAsFactors=FALSE)
    expect_equal(charts_list, expected_df)
})

test_that("checking that revisualize works", {
  server <- epivizrServer::createServer()
  data_mgr <- epivizrData::createMgr(server)
  chart_mgr <- EpivizChartMgr$new(server)
  
  gr <- GenomicRanges::GRanges(seqnames="chr1", 
                               ranges=IRanges::IRanges(start=seq(1,100,by=25), width=1), 
                               score1=rnorm(length(seq(1,100,by=25))),
                               score2=rnorm(length(seq(1,100,by=25))))
  
  ms_obj <- data_mgr$add_measurements(gr, "ms1", type="bp", send_request=FALSE)
  ms <- ms_obj$get_measurements()[2]
  
  chart_mgr$register_chart_type("LineChart", "epiviz.plugins.charts.LineTrack")
  chart_mgr$register_chart_type("BlockChart", "epiviz.plugins.charts.BlocksTrack")
  
  chart1 <- chart_mgr$visualize("BlockChart", datasource=ms_obj)
  
  chart_obj <- chart_mgr$visualize("LineChart", measurements=ms)
  chart_obj_rev <- chart_mgr$revisualize(chart_type = "BlockChart", chart=chart_obj)
  
  expect_is(chart_obj, "EpivizChart")
  expect_equal(chart_obj$.measurements, ms)
  expect_equal(chart_obj_rev$.measurements, ms)
  expect_equal(chart_obj$.type, "epiviz.plugins.charts.LineTrack")
  expect_equal(chart_obj_rev$.type, "epiviz.plugins.charts.BlocksTrack")
})
