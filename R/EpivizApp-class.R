#' Class managing connection to epiviz application.
#' 
#' @field server An object of class \code{\link[epivizrServer]{EpivizServer}} used to communicate with epiviz app.
#' @field data_mgr An object of class \code{\link[epivizrData]{EpivizData}} used to serve data to epiviz app.
#' @field chart_mgr An object of class \code{EpivizChartMgr} used to manage charts added to epiviz app session.
#' 
#' @importClassesFrom epivizrServer EpivizServer
#' @importClassesFrom epivizrData EpivizDataMgr EpivizMeasurement EpivizData
#' @import GenomicRanges
#' @include EpivizChartMgr-class.R
EpivizApp <- setRefClass("EpivizApp",
  fields=list(
    .url="character",
    .non_interactive="logical",
    server="EpivizServer",
    data_mgr="EpivizDataMgr",
    chart_mgr="EpivizChartMgr"
  ),
  methods=list(
   show=function() {
     "Print information about app connection."
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
    "Get object of class \\code{\\link[epivizrData]{EpivizData}} used as a data source
    in a given chart.
    
    \\describe{
      \\item{chart_id_or_object}{An object of class \\code{EpivizChart} or an id for
        a chart loaded to the epiviz app.}
      \\item{index}{Index into \\code{.measurements}} list of chart object to obtain data object for.
    }"
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
    "Visualize data on epiviz app using its default chart type. Measurements from the \\code{data-object} 
    are first added to the epiviz app using the \\code{add_measurements} method for class 
    \\code{\\link[epivizrData]{EpivizData}}. See documentation for \\code{\\link[epivizrData]{register}}
    for information on supported data types and the \\code{\\link[epivizrData]{EpivizData}} class
    encapsulating this type of data. Once measurements are loaded, the \\code{\\link{plot}} method
    of class \\code{\\link{EpivizChartMgr}} is used to plot the data, using the default chart type
    for this type of data.
    
    \\describe{
      \\item{data_object}{An object to plot in epiviz app.}
      \\item{...}{Additional arguments passed to \\code{add_measurements} method for class
        \\code{\\link[epivizrData]{EpivizData}}}
    }"
    ms_obj <- .self$data_mgr$add_measurements(data_object, ...)
    .self$chart_mgr$plot(ms_obj)
  }
)

# # navigation methods
EpivizApp$methods(
  navigate=function(chr, start, end) {
    'Navigate to given position on the epiviz app.'
    callback <- function(response_data) {
      invisible()
    }
    request_data=list(action="navigate",
      range=epivizrServer::json_writer(
        list(seqName=chr,start=start,end=end)))
    .self$server$send_request(request_data, callback)
  },
  get_current_location=function(callback) {
    'Obtain current genome location on epiviz app and evaluate callback
    function on result.
    
    \\describe{
      \\item{callback}{A callback function to evaluate on response data. 
        Response data will be a list with slots \\code{seqName}, \\code{start}
        and \\code{end}}
    }'
    request_data=list(action="getCurrentLocation")
    .self$server$send_request(request, callback)
  },
  slideshow=function(granges, n=length(granges)) {
    'Navigate on epiviz app successively to given positions.
    
    \\describe{
      \\item{granges}{An object of class \\code{\\link{GenomicRanges}} indicating
        set of genomic regions to navigate in epiviz app.}
      \\item{n}{(integer) The number of regions in \\code{granges} to navigate to.}
    }'
    if (!is(granges, "GenomicRanges"))
      stop(("'granges' must be a 'GenomicRanges' object"))

    n <- min(n, length(granges))
    ind <- seq(len=n)
    chr <- as.character(seqnames(granges)[ind])
    start <- start(granges)[ind]
    end <- end(granges)[ind]
    for (i in ind) {
      cat("Region", i, "of", n, ". Press key to continue (ESC to stop)...\n")
      if (!.self$.non_interactive)
        readLines(n=1)
      .self$navigate(chr=chr[i], start=start[i], end=end[i])
      tryCatch(.self$server$service(.self$.non_interactive), interrupt=function(int) invisible())
    }
    invisible()
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
