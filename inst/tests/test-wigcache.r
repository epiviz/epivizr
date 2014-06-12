context("fetch wig")

sendRequest=getOption("epivizrTestSendRequest")

test_that("cache mgmt works", {
  fl <- system.file("tests", "test.bw", package = "rtracklayer")
  ms <- register(BigWigFile(fl))

  expect_is(ms, "EpivizWigData")
  expect_equal(length(ms$cache$cacheRange), 0)

  query <- GRanges("chr2", IRanges(300, 400))
  obj <- import.bw(BigWigFile(fl))
  ms$getRows(query, NULL)

  rng <- resize(query, width=3*width(query), fix="center")

  print(subsetByOverlaps(obj, rng)); print(ms$object)
  expect_equal(as(ms$object, "GRanges"), subsetByOverlaps(obj, rng))
  expect_equal(ms$cache$cacheRange, rng)
})

