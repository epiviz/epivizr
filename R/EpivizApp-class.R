EpivizApp <- setRefClass("EpivizApp",
  fields=list(
    .url="character",
    server="EpivizServer",
    data_mgr="EpivizDataMgr",
    chart_mgr="EpivizChartMgr"
  ),
  methods=list(
   show=function() {
      cat("Epiviz App connection:\n")
      cat("Server: ")
      server$show(); cat("\n")

      st <- .self$chart_mgr$list_charts()
      if (!is.null(st)) {
        cat("Charts:\n")
        print(st); cat("\n")
      }

      st <- .self$data_mgr$list_measurements()
      if (length(st)>0) {
        cat("Measurements:\n")
        print(st); cat("\n")
      }
   }
  )
)

# general plot method
EpivizApp$methods(
  get_ms_object = function(chart_id_or_object, index=1) {
    chart_object <- .self$chart_mgr$.get_chart_object(chart_id_or_object)
    measurements <- chart_object$.measurements
    if (index < 1 || index > length(measurements)) {
      stop("'index' out of range")
    }
    measurement <- measurements[[index]]
    datasource <- measurement@datasourceId
    .self$data_mgr$.find_datasource(datasource)
  },
  plot = function(data_object, ...) {
    ms_obj <- .self$data_mgr$add_measurements(data_object, ...)
    .self$chart_mgr$plot(ms_obj)
  }
)

# # session management methods
# EpivizDeviceMgr$methods(list(
#   bindToServer=function() {
#     server$bindManager(.self)
#   },
#   isClosed=function() {
#     'check if connection is closed'
#     server$isClosed()
#   },
#   openBrowser=function(url=NULL) {
#     closeOnError <- FALSE
#     if (server$isClosed()) {
#       closeOnError <- TRUE
#       if (verbose) {
#         epivizrMsg("Starting epiviz websocket connection")
#       }
#       tryCatch(server$startServer(),
#                error=function(e) stop(e))
#     }
# 
#     if (nonInteractive) {
#       return(invisible())
#     }
# 
#     if (verbose) {
#       epivizrMsg("Opening browser")
#     }
# 
#     tryCatch({
#       if (missing(url) || is.null(url)) {
#         browseURL(.self$url)
#       } else {
#         browseURL(url)
#       }
# 
#       if (daemonized())
#         return(invisible())
# 
#       if (verbose) {
#         epivizrMsg("Servicing websocket until connected")
#       }
# 
#       ptm <- proc.time()
#       while(!server$socketConnected && (proc.time()-ptm)[2] * 1000 <= 30) {
#         service(verbose=FALSE)
#       }
#       if (!server$socketConnected) {
#         stop("[epivizr] Error opening connection. UI unable to connect to websocket server.")
#       }
#     }, error=function(e) {
#       if (closeOnError) server$stopServer()
#       stop(e)
#     })
#   },
#   service=function(verbose=TRUE) {
#     if (verbose && !(nonInteractive || daemonized())) {
#       epivizrMsg("Serving Epiviz, escape to continue interactive session...")
#     }
# 
#     server$service(nonInteractive)
#   },
#   stopService=function() {
#     server$stopService()
#   },
#   startServer=function() {
#     epivizrMsg("Starting websocket server...")
#     server$startServer()
#   },
#   stopServer=function() {
#     'stop epiviz connection'
#     .self$rmAllCharts(which="all")
#     .self$waitToClearRequests()
#     .self$rmAllMeasurements(which="all")
#     .self$waitToClearRequests()
#     .self$clearDeviceList()
#     server$stopServer()
#     invisible()
#   })
# )
# 
# # navigation methods
# EpivizDeviceMgr$methods(list(
#   refresh=function() {
#     'refresh browser'
#     server$refresh()
#   },
#   navigate=function(chr, start, end) {
#     'navigate to given position'
#     callback <- function(data) {
#       invisible(NULL)
#     }
#     requestId <- callbackArray$append(callback)
#     request=list(type="request",
#                  requestId=requestId,
#                  data=list(action="navigate",
#                            range=toJSON(list(seqName=chr,start=start,end=end))))
#     server$sendRequest(request)
#   },
#   getCurrentLocation=function(callback) {
#     requestId <- callbackArray$append(callback)
#     request <- list(type="request",
#                     requestId=requestId,
#                     data=list(action="getCurrentLocation"))
#     server$sendRequest(request)
#   },
#   slideshow=function(granges, n=length(granges)) {
#     'navidate to successive positions'
#     if (!is(granges, "GenomicRanges"))
#       stop(("'granges' must be a 'GenomicRanges' object"))
# 
#     n <- min(n, length(granges))
#     ind <- seq(len=n)
#     chr <- as.character(seqnames(granges)[ind])
#     start <- start(granges)[ind]
#     end <- end(granges)[ind]
#     for (i in ind) {
#       cat("Region", i, "of", n, ". Press key to continue (ESC to stop)...\n")
#       if (!nonInteractive)
#         readLines(n=1)
#       navigate(chr=chr[i], start=start[i], end=end[i])
#       tryCatch(service(), interrupt=function(int) invisible(NULL))
#     }
#     invisible(NULL)
#   }
# ))
# 
