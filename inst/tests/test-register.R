context("register measurement")

openBrowser=getOption("epivizrTestSendRequest")

test_that("register measurement works for block", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  dev <- epivizr:::register(gr)
  expect_true(validObject(dev))

  expect_is(dev, "EpivizBlockData")
  expect_is(dev$object, "GIntervalTree")

  expect_equal(as(dev$object, "GRanges"), gr)

  expect_true(is.null(dev$columns))
})

test_that("register works for bp data", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1),score=rnorm(10))
  dev <- epivizr::register(gr, columns="score", type="bp")
  expect_true(validObject(dev))

  expect_is(dev, "EpivizBpData")
  expect_is(dev$object, "GIntervalTree")
  
  expect_equal(as(dev$object, "GRanges"), unname(gr))
  expect_equal(dev$columns, "score") 
  rng=range(pretty(range(gr$score)))
  expect_equal(dev$ylim, cbind(score=rng))
})

test_that("register works for SummarizedExperiment", {
  sset <- makeSExp()
  dev <- epivizr::register(sset, columns=c("A","B"), assay="counts2")
  expect_true(validObject(dev))

  order <- order(start(rowData(sset)))
  sset <- sset[order,]
  
  expect_is(dev, "EpivizFeatureData")
  expect_is(dev$object, "SummarizedExperiment")
  gr <- as(rowData(dev$object), "GRanges")
  expect_false(is.null(gr$PROBEID))
  expect_false(is.null(gr$SYMBOL))
  gr$PROBEID <- NULL
  gr$SYMBOL <- NULL
  expect_identical(gr, unname(rowData(sset)))

  expect_identical(assays(dev$object), assays(sset))
  expect_identical(colData(dev$object), colData(sset))

  columns=c("A","B")
  expect_identical(dev$columns, columns)
  emat <- assay(sset,"counts2")[,c("A","B")]
  mat <- assay(dev$object,"counts2")[,c("A","B")]
  expect_equal(emat, mat)

  rngs <- apply(emat, 2, function(x) range(pretty(range(x))))
  expect_equal(dev$ylim, rngs, check.attributes=FALSE)
})

test_that("register works for ExpressionSet", {
  eset <- makeEset()
  dev <- epivizr::register(eset, columns=c("SAMP_1", "SAMP_2"))
  expect_true(validObject(dev))

  expect_is(dev, "EpivizFeatureData")
  expect_is(dev$object, "SummarizedExperiment")

  obj <- dev$object
  gr <- rowData(obj)

  m <- match(gr$PROBEID, featureNames(eset))
  mat <- assay(obj)

  expect_equal(exprs(eset)[m,"SAMP_1"], mat[,"SAMP_1"], check.names=FALSE, check.attributes=FALSE)
  expect_equal(exprs(eset)[m,"SAMP_2"], mat[,"SAMP_2"], check.names=FALSE, check.attributes=FALSE)

  rngs <- apply(exprs(eset)[m,c("SAMP_1","SAMP_2")], 2, function(x) range(pretty(range(x))))
  expect_equal(dev$ylim, rngs, check.attributes=FALSE)
})

