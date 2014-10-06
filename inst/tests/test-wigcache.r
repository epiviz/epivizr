context("fetch wig")

sendRequest=getOption("epivizrTestSendRequest")

test_that("cache mgmt works", {
  fl <- BigWigFile(system.file("tests", "test.bw", package = "rtracklayer"))
  ms <- register(fl)

  expect_is(ms, "EpivizWigData")
  expect_equal(length(ms$cache$cacheRange), 0)

  query <- GRanges("chr2", IRanges(300, 400))
  ms$getRows(query, NULL)

  rng <- resize(query, width=3*width(query), fix="center")
  obj <- import.bw(fl, which=rng, as="GRanges")

  expect_equal(as(ms$object, "GRanges"), obj)
  expect_equal(ms$cache$cacheRange, rng)

  cat("first call done\n")
  
  # no change since this query is in cache
  query <- GRanges("chr2", IRanges(400,500))
  ms$getRows(query, NULL)
  expect_equal(as(ms$object, "GRanges"), obj)
  expect_equal(ms$cache$cacheRange, rng)

  cat("second call done\n")
  
  # new stuff on the right
  query <- GRanges("chr2", IRanges(500, 600))
  rng2 <- setdiff(query, rng)
  rng2 <- resize(rng2, fix="start", width=2*width(rng2))
  
  ms$getRows(query, NULL)
  obj2 <- import.bw(fl, which=rng2, as="GRanges")
  obj2 <- c(obj, obj2)
  rng2 <- c(rng, rng2)
  
  expect_equal(as(ms$object, "GRanges"), obj2)
  expect_equal(ms$cache$cacheRange, rng2)

  cat("third call done\n")

  # new stuff on the left
  query <- GRanges("chr2", IRanges(200,300))
  rng3 <- setdiff(query, rng2)
  rng3 <- resize(rng3, fix="end", width=2*width(rng3))

  ms$getRows(query, NULL)
  obj3 <- import.bw(fl, which=rng3, as="GRanges")
  obj3 <- c(obj3, obj2)
  rng3 <- c(rng3, rng2)

  expect_equal(as(ms$object, "GRanges"), obj3)
  expect_equal(ms$cache$cacheRange, rng3)
  cat("fourth call done\n")

  # no change since this query is in cache
  query <- GRanges("chr2", IRanges(300, 400))
  ms$getRows(query, NULL)
  expect_equal(as(ms$object, "GRanges"), obj3)
  expect_equal(ms$cache$cacheRange, rng3)
  cat("fifth call done\n")

  # zoom out, which should replace the whole thing
  query <- GRanges("chr2", IRanges(100,1000))
  rng <- resize(query, width=3*width(query), fix="center")
  if (start(rng) < 1) {
      start(rng) <- 1
  }
  if (end(rng) > seqlengths(seqinfo(fl))["chr2"]) {
      end(rng) <- seqlengths(seqinfo(fl))["chr2"]
  }
  obj <- import.bw(fl, which=rng, as="GRanges")
  ms$getRows(query, NULL)

  expect_equal(as(ms$object, "GRanges"), obj)
  expect_equal(ms$cache$cacheRange, rng)
})

