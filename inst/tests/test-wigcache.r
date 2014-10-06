context("fetch wig")

sendRequest=getOption("epivizrTestSendRequest")
fl <- BigWigFile(system.file("tests", "test.bw", package = "rtracklayer"))

test_that("cache mgmt works with import.bw", {
  ms <- register(fl, windowSizes=0L)
  
  expect_is(ms, "EpivizWigData")
  expect_is(ms$caches, "list")
  expect_equal(length(ms$caches), 1)
  expect_equal(ms$currentCache, 1)
  expect_is(ms$cache(), "EpivizWigCache")
  
  expect_equal(length(ms$cache()$cacheRange), 0)

  query <- GRanges("chr2", IRanges(500, 600))
  res <- ms$getRows(query, NULL)

  rng <- resize(query, width=3*width(query), fix="center")
  obj <- import.bw(fl, which=rng, as="GRanges")

  tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj, select="all"))
  expectedRes <- list(
      globalStartIndex=tmp[1],
      useOffset=TRUE,
      values=list(
          id=list(tmp),
          start=list(c(start(obj)[tmp[1]], diff(start(obj)[tmp]))),
          end=list(c(end(obj)[tmp[1]], diff(end(obj)[tmp]))),
          metadata=NULL))
  
  expect_equal(as(ms$object, "GRanges"), obj)
  expect_equal(ms$cache()$cacheRange, rng)
  expect_equal(res, expectedRes)
})

test_that("import.bw: hit cache", {
  ms <- register(fl, windowSizes=0L)
  query <- GRanges("chr2", IRanges(500, 600))
  ms$getRows(query, NULL)

  rng <- resize(query, width=3*width(query), fix="center")
  obj <- import.bw(fl, which=rng, as="GRanges")

  # no change since this query is in cache
  query <- GRanges("chr2", IRanges(600,700))
  res <- ms$getRows(query, NULL)
  
  tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj, select="all"))
  expectedRes <- list(
      globalStartIndex=tmp[1],
      useOffset=TRUE,
      values=list(
          id=tmp,
          start=c(start(obj)[tmp[1]], diff(start(obj)[tmp])),
          end=c(end(obj)[tmp[1]], diff(end(obj)[tmp])),
          metadata=NULL))
  
  expect_equal(as(ms$object, "GRanges"), obj)
  expect_equal(ms$cache()$cacheRange, rng)
  expect_equal(res, expectedRes)
}  )

test_that("import.bw: extend right", {
    ms <- register(fl, windowSizes=0L)
    query <- GRanges("chr2", IRanges(500, 600))
    ms$getRows(query, NULL)

    rng <- resize(query, width=3*width(query), fix="center")
    obj <- import.bw(fl, which=rng, as="GRanges")

    query <- GRanges("chr2", IRanges(600,700))
    ms$getRows(query, NULL)

  # new stuff on the right
    query <- GRanges("chr2", IRanges(700, 800))
  
    rng2 <- setdiff(query, rng)
    rng2 <- resize(rng2, fix="start", width=2*width(rng2))
  
    res <- ms$getRows(query, NULL)
    obj2 <- import.bw(fl, which=rng2, as="GRanges")
    obj2 <- c(obj, obj2)
    start(rng2) <- start(rng)

    tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj2, select="all"))
    expectedRes <- list(
      globalStartIndex=tmp[1],
      useOffset=TRUE,
      values=list(
          id=tmp,
          start=c(start(obj2)[tmp[1]], diff(start(obj2)[tmp])),
          end=c(end(obj2)[tmp[1]], diff(end(obj2)[tmp])),
          metadata=NULL))
    
    expect_equal(as(ms$object, "GRanges"), obj2)
    expect_equal(ms$cache()$cacheRange, rng2)
    expect_equal(res, expectedRes)
})

test_that("import.bw: extend left", {
    ms <- register(fl, windowSizes=0L)
    query <- GRanges("chr2", IRanges(500, 600))
    ms$getRows(query, NULL)
    
    rng <- resize(query, width=3*width(query), fix="center")
    obj <- import.bw(fl, which=rng, as="GRanges")
    
    query <- GRanges("chr2", IRanges(600,700))
    ms$getRows(query, NULL)
    
                                        # new stuff on the right
    query <- GRanges("chr2", IRanges(700, 800))
  
    rng2 <- setdiff(query, rng)
    rng2 <- resize(rng2, fix="start", width=2*width(rng2))
  
    res <- ms$getRows(query, NULL)
    obj2 <- import.bw(fl, which=rng2, as="GRanges")
    obj2 <- c(obj, obj2)
    start(rng2) <- start(rng)

  # new stuff on the left
  query <- GRanges("chr2", IRanges(300,500))
  rng3 <- setdiff(query, rng2)
  rng3 <- resize(rng3, fix="end", width=2*width(rng3))

  res <- ms$getRows(query, NULL)
  obj3 <- import.bw(fl, which=rng3, as="GRanges")
  indexOffset <- length(obj3) + 1
  obj3 <- c(obj3, obj2)
  end(rng3) <- end(rng2)

  tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj3, select="all"))
  expectedRes <- list(
      globalStartIndex=tmp[1] - indexOffset,
      useOffset=TRUE,
      values=list(
          id=tmp - indexOffset,
          start=c(start(obj3)[tmp[1]], diff(start(obj3)[tmp])),
          end=c(end(obj3)[tmp[1]], diff(end(obj3)[tmp])),
          metadata=NULL))

  expect_equal(as(ms$object, "GRanges"), obj3)
  expect_equal(ms$cache()$cacheRange, rng3)
  expect_equal(res, expectedRes)
})

test_that("import.bw: hit cache again", {
        ms <- register(fl, windowSizes=0L)
    query <- GRanges("chr2", IRanges(500, 600))
    ms$getRows(query, NULL)
    
    rng <- resize(query, width=3*width(query), fix="center")
    obj <- import.bw(fl, which=rng, as="GRanges")
    
    query <- GRanges("chr2", IRanges(600,700))
    ms$getRows(query, NULL)
    
                                        # new stuff on the right
    query <- GRanges("chr2", IRanges(700, 800))
  
    rng2 <- setdiff(query, rng)
    rng2 <- resize(rng2, fix="start", width=2*width(rng2))
  
    res <- ms$getRows(query, NULL)
    obj2 <- import.bw(fl, which=rng2, as="GRanges")
    obj2 <- c(obj, obj2)
    start(rng2) <- start(rng)

  # new stuff on the left
  query <- GRanges("chr2", IRanges(300,500))
  rng3 <- setdiff(query, rng2)
  rng3 <- resize(rng3, fix="end", width=2*width(rng3))

  res <- ms$getRows(query, NULL)
  obj3 <- import.bw(fl, which=rng3, as="GRanges")
  indexOffset <- length(obj3) + 1
  obj3 <- c(obj3, obj2)
  end(rng3) <- end(rng2)

  # no change since this query is in cache
  query <- GRanges("chr2", IRanges(500, 600))
  res <- ms$getRows(query, NULL)

  tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj3, select="all"))
  expectedRes <- list(
      globalStartIndex=tmp[1] - indexOffset,
      useOffset=TRUE,
      values=list(
          id=list(tmp - indexOffset),
          start=list(c(start(obj3)[tmp[1]], diff(start(obj3)[tmp]))),
          end=list(c(end(obj3)[tmp[1]], diff(end(obj3)[tmp]))),
          metadata=NULL))
  
  expect_equal(as(ms$object, "GRanges"), obj3)
  expect_equal(ms$cache()$cacheRange, rng3)
  expect_equal(res, expectedRes)
})  

test_that("import.bw: zoom out", {
            ms <- register(fl, windowSizes=0L)
    query <- GRanges("chr2", IRanges(500, 600))
    ms$getRows(query, NULL)
    
    rng <- resize(query, width=3*width(query), fix="center")
    obj <- import.bw(fl, which=rng, as="GRanges")
    
    query <- GRanges("chr2", IRanges(600,700))
    ms$getRows(query, NULL)
    
                                        # new stuff on the right
    query <- GRanges("chr2", IRanges(700, 800))
  
    rng2 <- setdiff(query, rng)
    rng2 <- resize(rng2, fix="start", width=2*width(rng2))
  
    res <- ms$getRows(query, NULL)
    obj2 <- import.bw(fl, which=rng2, as="GRanges")
    obj2 <- c(obj, obj2)
    start(rng2) <- start(rng)

  # new stuff on the left
  query <- GRanges("chr2", IRanges(300,500))
  rng3 <- setdiff(query, rng2)
  rng3 <- resize(rng3, fix="end", width=2*width(rng3))

  res <- ms$getRows(query, NULL)
  obj3 <- import.bw(fl, which=rng3, as="GRanges")
  indexOffset <- length(obj3) + 1
  obj3 <- c(obj3, obj2)
  end(rng3) <- end(rng2)

  # no change since this query is in cache
  query <- GRanges("chr2", IRanges(500, 600))
  res <- ms$getRows(query, NULL)

                                        # zoom out, which should replace the whole thing
  query <- GRanges("chr2", IRanges(1,2000))
  rng <- resize(query, width=3*width(query), fix="center")
  if (start(rng) < 1) {
      start(rng) <- 1
  }
  if (end(rng) > seqlengths(seqinfo(fl))["chr2"]) {
      end(rng) <- seqlengths(seqinfo(fl))["chr2"]
  }
  obj <- import.bw(fl, which=rng, as="GRanges")
  res <- ms$getRows(query, NULL)

    tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj, select="all"))
  expectedRes <- list(
      globalStartIndex=tmp[1],
      useOffset=TRUE,
      values=list(
          id=tmp,
          start=c(start(obj)[tmp[1]], diff(start(obj)[tmp])),
          end=c(end(obj)[tmp[1]], diff(end(obj)[tmp])),
          metadata=NULL))

  expect_equal(as(ms$object, "GRanges"), obj)
  expect_equal(ms$cache()$cacheRange, rng)
            expect_equal(res, expectedRes)
})

test_that("cache mgmt works with summary", {
    windowSizes <- windowSize <- 100L
  ms <- register(fl, windowSizes=windowSizes)

  expect_is(ms, "EpivizWigData")
  expect_equal(length(ms$cache()$cacheRange), 0)

  query <- GRanges("chr2", IRanges(500, 600))
  res <- ms$getRows(query, NULL)

  rng <- resize(query, width=3*width(query), fix="center")
  obj <- summary(fl, which=rng, size=ceiling(width(rng) / windowSize))[[1]]

  tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj, select="all"))
    
  expectedRes <- list(
      globalStartIndex=tmp[1],
      useOffset=TRUE,
      values=list(
          id=tmp,
          start=c(start(obj)[tmp[1]], diff(start(obj)[tmp])),
          end=c(end(obj)[tmp[1]], diff(end(obj)[tmp])),
          metadata=NULL))
    
  expect_equal(as(ms$object, "GRanges"), obj)
  expect_equal(ms$cache()$cacheRange, rng)
  expect_equal(res, expectedRes)
})

test_that("summary: hit cache", {
    windowSizes <- windowSize <- 100L
  ms <- register(fl, windowSizes=windowSizes)
  query <- GRanges("chr2", IRanges(500, 600))
  ms$getRows(query, NULL)

  rng <- resize(query, width=3*width(query), fix="center")
  obj <- summary(fl, which=rng, size=ceiling(width(rng) / windowSize))[[1]]

  # no change since this query is in cache
  query <- GRanges("chr2", IRanges(600,700))
  res <- ms$getRows(query, NULL)

  tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj, select="all"))
  expectedRes <- list(
      globalStartIndex=tmp[1],
      useOffset=TRUE,
      values=list(
          id=tmp,
          start=c(start(obj)[tmp[1]], diff(start(obj)[tmp])),
          end=c(end(obj)[tmp[1]], diff(end(obj)[tmp])),
          metadata=NULL))

    expect_equal(as(ms$object, "GRanges"), obj)
  expect_equal(ms$cache()$cacheRange, rng)
      expect_equal(res, expectedRes)
})

test_that("summary: extend right", {
    windowSizes <- windowSize <- 100L
  ms <- register(fl, windowSizes=windowSizes)
  query <- GRanges("chr2", IRanges(500, 600))
  ms$getRows(query, NULL)

  rng <- resize(query, width=3*width(query), fix="center")
  obj <- summary(fl, which=rng, size=ceiling(width(rng) / windowSize))[[1]]

  query <- GRanges("chr2", IRanges(600,700))
  ms$getRows(query, NULL)
    
  # new stuff on the right
  query <- GRanges("chr2", IRanges(700, 800))
  rng2 <- setdiff(query, rng)
  rng2 <- resize(rng2, fix="start", width=2*width(rng2))
  
  res <- ms$getRows(query, NULL)
  obj2 <- summary(fl, which=rng2, size=ceiling(width(rng2) / windowSize))[[1]]
  obj2 <- c(obj, obj2)
  start(rng2) <- start(rng)

        tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj2, select="all"))
    expectedRes <- list(
      globalStartIndex=tmp[1],
      useOffset=TRUE,
      values=list(
          id=tmp,
          start=c(start(obj2)[tmp[1]], diff(start(obj2)[tmp])),
          end=c(end(obj2)[tmp[1]], diff(end(obj2)[tmp])),
          metadata=NULL))
    

  expect_equal(as(ms$object, "GRanges"), obj2)
  expect_equal(ms$cache()$cacheRange, rng2)
    expect_equal(res, expectedRes)
})

test_that("summary: extend left", {
    windowSizes <- windowSize <- 100L
  ms <- register(fl, windowSizes=windowSizes)
  query <- GRanges("chr2", IRanges(500, 600))
  ms$getRows(query, NULL)

  rng <- resize(query, width=3*width(query), fix="center")
  obj <- summary(fl, which=rng, size=ceiling(width(rng) / windowSize))[[1]]

  query <- GRanges("chr2", IRanges(600,700))
  ms$getRows(query, NULL)
    
  # new stuff on the right
  query <- GRanges("chr2", IRanges(700, 800))
  rng2 <- setdiff(query, rng)
  rng2 <- resize(rng2, fix="start", width=2*width(rng2))
  
  ms$getRows(query, NULL)
  obj2 <- summary(fl, which=rng2, size=ceiling(width(rng2) / windowSize))[[1]]
  obj2 <- c(obj, obj2)
  start(rng2) <- start(rng)
    
  # new stuff on the left
  query <- GRanges("chr2", IRanges(300,500))
  rng3 <- setdiff(query, rng2)
  rng3 <- resize(rng3, fix="end", width=2*width(rng3))
    
  res <- ms$getRows(query, NULL)
  obj3 <- summary(fl, which=rng3, size=ceiling(width(rng3) / windowSize))[[1]]
    indexOffset <- length(obj3) + 1
    
  obj3 <- c(obj3, obj2)
  end(rng3) <- end(rng2)

      tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj3, select="all"))
  expectedRes <- list(
      globalStartIndex=tmp[1] - indexOffset,
      useOffset=TRUE,
      values=list(
          id=tmp - indexOffset,
          start=c(start(obj3)[tmp[1]], diff(start(obj3)[tmp])),
          end=c(end(obj3)[tmp[1]], diff(end(obj3)[tmp])),
          metadata=NULL))

  expect_equal(as(ms$object, "GRanges"), obj3)
  expect_equal(ms$cache()$cacheRange, rng3)
      expect_equal(res, expectedRes)
})

test_that("summary: hit cache again", {
        windowSizes <- windowSize <- 100L
  ms <- register(fl, windowSizes=windowSizes)
  query <- GRanges("chr2", IRanges(500, 600))
  ms$getRows(query, NULL)

  rng <- resize(query, width=3*width(query), fix="center")
  obj <- summary(fl, which=rng, size=ceiling(width(rng) / windowSize))[[1]]

  query <- GRanges("chr2", IRanges(600,700))
  ms$getRows(query, NULL)
    
  # new stuff on the right
  query <- GRanges("chr2", IRanges(700, 800))
  rng2 <- setdiff(query, rng)
  rng2 <- resize(rng2, fix="start", width=2*width(rng2))
  
  ms$getRows(query, NULL)
  obj2 <- summary(fl, which=rng2, size=ceiling(width(rng2) / windowSize))[[1]]
  obj2 <- c(obj, obj2)
  start(rng2) <- start(rng)
    
  # new stuff on the left
  query <- GRanges("chr2", IRanges(300,500))
  rng3 <- setdiff(query, rng2)
  rng3 <- resize(rng3, fix="end", width=2*width(rng3))
    
  ms$getRows(query, NULL)
  obj3 <- summary(fl, which=rng3, size=ceiling(width(rng3) / windowSize))[[1]]
indexOffset <- length(obj3) + 1
            
  obj3 <- c(obj3, obj2)
  end(rng3) <- end(rng2)

  # no change since this query is in cache
  query <- GRanges("chr2", IRanges(500, 600))
  res <- ms$getRows(query, NULL)

          tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj3, select="all"))
  expectedRes <- list(
      globalStartIndex=tmp[1] - indexOffset,
      useOffset=TRUE,
      values=list(
          id=tmp - indexOffset,
          start=c(start(obj3)[tmp[1]], diff(start(obj3)[tmp])),
          end=c(end(obj3)[tmp[1]], diff(end(obj3)[tmp])),
          metadata=NULL))
  

  expect_equal(as(ms$object, "GRanges"), obj3)
  expect_equal(ms$cache()$cacheRange, rng3)
          expect_equal(res, expectedRes)
    })

test_that("summary: zoom out", {
            windowSizes <- windowSize <- 100L
  ms <- register(fl, windowSizes=windowSizes)
  query <- GRanges("chr2", IRanges(500, 600))
  ms$getRows(query, NULL)

  rng <- resize(query, width=3*width(query), fix="center")
  obj <- summary(fl, which=rng, size=ceiling(width(rng) / windowSize))[[1]]

  query <- GRanges("chr2", IRanges(600,700))
  ms$getRows(query, NULL)
    
  # new stuff on the right
  query <- GRanges("chr2", IRanges(700, 800))
  rng2 <- setdiff(query, rng)
  rng2 <- resize(rng2, fix="start", width=2*width(rng2))
  
  ms$getRows(query, NULL)
  obj2 <- summary(fl, which=rng2, size=ceiling(width(rng2) / windowSize))[[1]]
  obj2 <- c(obj, obj2)
  start(rng2) <- start(rng)
    
  # new stuff on the left
  query <- GRanges("chr2", IRanges(300,500))
  rng3 <- setdiff(query, rng2)
  rng3 <- resize(rng3, fix="end", width=2*width(rng3))
    
  ms$getRows(query, NULL)
  obj3 <- summary(fl, which=rng3, size=ceiling(width(rng3) / windowSize))[[1]]
  obj3 <- c(obj3, obj2)
  end(rng3) <- end(rng2)

  # no change since this query is in cache
  query <- GRanges("chr2", IRanges(500, 600))
  ms$getRows(query, NULL)

  # zoom out, which should replace the whole thing
  query <- GRanges("chr2", IRanges(1,2000))
  rng <- resize(query, width=3*width(query), fix="center")
  if (start(rng) < 1) {
      start(rng) <- 1
  }
  if (end(rng) > seqlengths(seqinfo(fl))["chr2"]) {
      end(rng) <- seqlengths(seqinfo(fl))["chr2"]
  }
  obj <- summary(fl, which=rng, size=ceiling(width(rng) / windowSize))[[1]]
  naIndex <- is.na(obj$score)
  if (any(naIndex)) {
      obj <- obj[!naIndex,]
  }
  res <- ms$getRows(query, NULL)

    tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj, select="all"))
  expectedRes <- list(
      globalStartIndex=tmp[1],
      useOffset=TRUE,
      values=list(
          id=tmp,
          start=c(start(obj)[tmp[1]], diff(start(obj)[tmp])),
          end=c(end(obj)[tmp[1]], diff(end(obj)[tmp])),
          metadata=NULL))
            
  expect_equal(as(ms$object, "GRanges"), obj)
  expect_equal(ms$cache()$cacheRange, rng)
            expect_equal(res, expectedRes)
})

test_that("cache mgmt works with cache list", {
    windowSizes <- c(0L, 100L)
    windowSize <- 100L
  ms <- register(fl, windowSizes=windowSizes, triggerValues=c(0L, 100L))

  expect_is(ms, "EpivizWigData")
    expect_is(ms$caches, "list")
    expect_equal(length(ms$caches), 2)
    expect_equal(ms$currentCache, 2)
    expect_is(ms$cache(), "EpivizWigCache")
    
  expect_equal(length(ms$cache()$cacheRange), 0)

  query <- GRanges("chr2", IRanges(500, 600))
  res <- ms$getRows(query, NULL)

  rng <- resize(query, width=3*width(query), fix="center")
  obj <- summary(fl, which=rng, size=ceiling(width(rng) / windowSize))[[1]]

  tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj, select="all"))
    
  expectedRes <- list(
      globalStartIndex=tmp[1],
      useOffset=TRUE,
      values=list(
          id=tmp,
          start=c(start(obj)[tmp[1]], diff(start(obj)[tmp])),
          end=c(end(obj)[tmp[1]], diff(end(obj)[tmp])),
          metadata=NULL))
    
  expect_equal(as(ms$object, "GRanges"), obj)
  expect_equal(ms$cache()$cacheRange, rng)
  expect_equal(res, expectedRes)
})

test_that("cacheList: extend left", {
    windowSizes <- c(0L, 100L)
    windowSize <- 100L
    ms <- register(fl, windowSizes=windowSizes, triggerValues=c(0L, 100L))

  query <- GRanges("chr2", IRanges(500, 600))
  ms$getRows(query, NULL)

  rng <- resize(query, width=3*width(query), fix="center")
  obj <- summary(fl, which=rng, size=ceiling(width(rng) / windowSize))[[1]]

  query <- GRanges("chr2", IRanges(600,700))
  ms$getRows(query, NULL)
    
  # new stuff on the right
  query <- GRanges("chr2", IRanges(700, 800))
  rng2 <- setdiff(query, rng)
  rng2 <- resize(rng2, fix="start", width=2*width(rng2))
  
  ms$getRows(query, NULL)
  obj2 <- summary(fl, which=rng2, size=ceiling(width(rng2) / windowSize))[[1]]
  obj2 <- c(obj, obj2)
  start(rng2) <- start(rng)
    
  # new stuff on the left
  query <- GRanges("chr2", IRanges(300,500))
  rng3 <- setdiff(query, rng2)
  rng3 <- resize(rng3, fix="end", width=2*width(rng3))
    
  res <- ms$getRows(query, NULL)
  obj3 <- summary(fl, which=rng3, size=ceiling(width(rng3) / windowSize))[[1]]
    indexOffset <- length(obj3) + 1
    
  obj3 <- c(obj3, obj2)
  end(rng3) <- end(rng2)

      tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj3, select="all"))
  expectedRes <- list(
      globalStartIndex=tmp[1] - indexOffset,
      useOffset=TRUE,
      values=list(
          id=tmp - indexOffset,
          start=c(start(obj3)[tmp[1]], diff(start(obj3)[tmp])),
          end=c(end(obj3)[tmp[1]], diff(end(obj3)[tmp])),
          metadata=NULL))

  expect_equal(as(ms$object, "GRanges"), obj3)
  expect_equal(ms$cache()$cacheRange, rng3)
      expect_equal(res, expectedRes)
})

test_that("cacheList: cacheSwitch", {
    cat("testing cacheSwitch\n")
    windowSizes <- c(0L, 100L)
    windowSize <- 100L
    ms <- register(fl, windowSizes=windowSizes, triggerValues=c(10L, 1000L))

    expect_equal(ms$currentCache, 2)
  query <- GRanges("chr2", IRanges(500, 600))
  ms$getRows(query, NULL)

    expect_equal(ms$currentCache, 1)
  rng <- resize(query, width=3*width(query), fix="center")
    obj <- import.bw(fl, which=rng, as="GRanges")    
#  obj <- summary(fl, which=rng, size=ceiling(width(rng) / windowSize))[[1]]

  query <- GRanges("chr2", IRanges(600,700))
 ms$getRows(query, NULL)
  
 # new stuff on the right
 query <- GRanges("chr2", IRanges(700, 800))
 rng2 <- setdiff(query, rng)
 rng2 <- resize(rng2, fix="start", width=2*width(rng2))
 
 ms$getRows(query, NULL)
    expect_equal(ms$currentCache, 1)
    obj2 <- import.bw(fl, which=rng2, as="GRanges")
# obj2 <- summary(fl, which=rng2, size=ceiling(width(rng2) / windowSize))[[1]]
  obj2 <- c(obj, obj2)
  start(rng2) <- start(rng)
    
  # new stuff on the left
  query <- GRanges("chr2", IRanges(300,500))
  rng3 <- setdiff(query, rng2)
  rng3 <- resize(rng3, fix="end", width=2*width(rng3))
    
  res <- ms$getRows(query, NULL)
    expect_equal(ms$currentCache, 1)
      obj3 <- import.bw(fl, which=rng3, as="GRanges")
# # obj3 <- summary(fl, which=rng3, size=ceiling(width(rng3) / windowSize))[[1]]
    indexOffset <- length(obj3) + 1
    
  obj3 <- c(obj3, obj2)
  end(rng3) <- end(rng2)
  #
      tmp <- subjectHits(GenomicRanges::findOverlaps(query, obj3, select="all"))
  expectedRes <- list(
      globalStartIndex=tmp[1] - indexOffset,
      useOffset=TRUE,
      values=list(
          id=tmp - indexOffset,
          start=c(start(obj3)[tmp[1]], diff(start(obj3)[tmp])),
          end=c(end(obj3)[tmp[1]], diff(end(obj3)[tmp])),
          metadata=NULL))
  
  expect_equal(as(ms$object, "GRanges"), obj3)
  expect_equal(ms$cache()$cacheRange, rng3)
      expect_equal(res, expectedRes)
})

