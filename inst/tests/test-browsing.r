context("browser commands")

test_that("refresh works", {
  tryCatch({
    mgr <- .startMGR()
    mgr$refresh()
  }, finally=mgr$stopServer())
})

test_that("navigate works", {
  tryCatch({
    mgr <- .startMGR()
    mgr$navigate(chr="chr10", start=2000, end=10000)
  }, finally=mgr$stopServer())
})