wait_until <- function(condition, timeout=3) {
  condition <- substitute(condition)
  ptm <- proc.time()
  while (!eval(condition, parent.frame()) && (proc.time() - ptm < timeout)["elapsed"]) {
    Sys.sleep(0.001)
  }
  expect_true(eval(condition, parent.frame()))
}
