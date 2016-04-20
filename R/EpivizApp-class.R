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
   },
   is_server_closed=function() {
     "Check if underlying server connection is closed."
     is.null(.self$server) || .self$server$is_closed()
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
    .self$server$wait_to_clear_requests()
    .self$chart_mgr$plot(ms_obj)
  }
)

# # navigation methods
EpivizApp$methods(
  navigate=function(chr, start, end) {
    'Navigate to given position on the epiviz app.'
    if (.self$is_server_closed()) {
      return(invisible())
    }
    
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
  slideshow=function(granges, n=length(granges), .callback=NULL) {
    'Navigate on epiviz app successively to given positions.
    
    \\describe{
      \\item{granges}{An object of class \\code{\\link{GenomicRanges}} indicating
        set of genomic regions to navigate in epiviz app.}
      \\item{n}{(integer) The number of regions in \\code{granges} to navigate to.}
      \\item{.callback}{(function) function to call after navigating to each region. Used for testing purposes.}
    }'
    if (!is(granges, "GenomicRanges"))
      stop(("'granges' must be a 'GenomicRanges' object"))

    n <- min(n, length(granges))
    ind <- seq(len=n)
    chr <- as.character(seqnames(granges)[ind])
    start <- start(granges)[ind]
    end <- end(granges)[ind]
    for (i in ind) {
      .self$navigate(chr=chr[i], start=start[i], end=end[i])
      if (!is.null(.callback) && is.function(.callback)) {
        .callback(chr[i], start[i], end[i])
      } else {
        if (.self$server$is_interactive()) {
          cat("Region", i, "of", n, ". Press key to continue (ESC to stop)...\n")
          readLines(n=1)
        }
      }
      tryCatch(.self$server$service(), interrupt=function(int) invisible())
    }
    invisible()
  }
)

# data update methods
EpivizApp$methods(
  update_measurements = function(ms_object, new_data_object, send_request = TRUE) {
    .self$data_mgr$update_measurements(ms_object, new_data_object, send_request = send_request)
    .self$chart_mgr$.redraw(send_request = send_request)
  }
)

# session management methods
EpivizApp$methods(
  .open_browser=function() {
    close_on_error <- FALSE
    if (.self$is_server_closed()) {
      close_on_error <- TRUE
      if (.self$server$.verbose) {
        cat("Starting epiviz websocket connection\n")
      }
      tryCatch(.self$server$start_server(),
               error=function(e) stop(e))
    }

    if (!.self$server$is_interactive()) {
      return(invisible())
    }

    if (.self$server$.verbose) {
      cat("Opening browser\n")
    }

    tryCatch({
      browseURL(.self$.url)
      
      if (.self$server$is_daemonized()) {
        return(invisible())
      }

      if (.self$server$.verbose) {
        cat("Servicing websocket until connected\n")
      }

      ptm <- proc.time()
      while(!.self$server$is_socket_connected() && (proc.time()-ptm)[2] * 1000 <= 30) {
        .self$service(verbose=FALSE)
      }
      if (!.self$server$is_socket_connected()) {
        stop("[epivizr] Error opening connection. UI unable to connect to websocket server.")
      }
    }, error=function(e) {
      if (closeOnError) .self$server$stop_server()
      stop(e)
    })
  },
  print_workspace=function(file_name=NULL, file_type="pdf"){
    callback=function(data) {
      cat("workspace is being saved as", file_type, "\n")
    }
    
    request_data <- list(action = "printWorkspace", fileName=file_name, fileType=file_type)
    .self$server$send_request(request_data, callback)
    invisible()
  },
  service=function(verbose=TRUE) {
    "Block interactive R session to service websocket requests."
    if (verbose && .self$server$is_interactive() && !.self$server$is_daemonized()) {
      cat("Serving Epiviz, escape to continue interactive session...\n")
    }

    .self$server$service()
  },
  .stop_service=function() {
    .self$server$stop_service()
  },
  stop_app=function() {
    'stop and clean connection to epiviz app.'
    .self$chart_mgr$rm_all_charts()
    .self$server$wait_to_clear_requests()
    .self$data_mgr$rm_all_measurements()
    .self$server$wait_to_clear_requests()
    .self$server$stop_server()
    invisible()
  }
)

