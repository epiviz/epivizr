.onLoad <- function(libname, pkgname) {
  if (!require(httpuv, quietly=TRUE)) {
    message("httpuv not found, needed to run epivizr")
  }

  tryCatch({
    if (is.function(httpuv::startDaemonizedServer))
      options(epivizrCanDaemonize=.Platform$OS.type == "unix")
  }, error=function(e) {
    options(epivizCanDaemonize=FALSE)
  })
}
