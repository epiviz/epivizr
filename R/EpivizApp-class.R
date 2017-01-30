.constructURL <- function(host=NULL, http_port=NULL, path = NULL, ws_host="localhost", ws_port=7123L,
                          use_devel=FALSE, debug=FALSE, 
                          chr="chr11", start=99800000, end=103383180,
                          workspace=NULL, scripts=NULL, gists=NULL, use_cookie=FALSE)
{
  if (is.null(host)) {
    host <- ifelse(use_devel,"epiviz-dev", "epiviz")
    host <- sprintf("http://%s.cbcb.umd.edu", host)
  }
  
  if (!is.null(http_port)) {
    port <- sprintf(":%d", http_port)
  } else {
    port <- ""
  }
  
  if (is.null(path)) {
    path <- "/index.php"
  }
  
  url <- paste0(host,port,path)
  controllerHost <- sprintf("ws://%s:%d", ws_host, ws_port)  
  url <- sprintf("%s?websocket-host[]=%s&", url, controllerHost)
  url <- paste0(url, sprintf("debug=%s&", ifelse(debug, "true", "false")))
  
  if (!is.null(workspace)) {
    url <- paste0(url,"ws=",workspace,"&")
  } else if (!is.null(chr) && !is.null(start) && !is.null(end)) {
    url <- paste0(url,
                  sprintf("seqName=%s&start=%d&end=%d&",
                          chr,
                          as.integer(start),
                          as.integer(end)))
  }
  
  if (!is.null(scripts)) {
    script_string <- paste(sprintf("script[]=%s&", scripts),collapse="")
    url <- paste0(url, script_string)
  }
  
  if (!is.null(gists)) {
    gist_string <- paste(sprintf("gist[]=%s&", gists), collapse="")
    url <- paste0(url, gist_string)
  }
  
  cookie_string <- sprintf("useCookie=%s&", ifelse(use_cookie, "true", "false"))
  url <- paste0(url, cookie_string)
  url
}

#' Class managing connection to epiviz application.
#' 
#' @field server An object of class \code{\link[epivizrServer]{EpivizServer}} used to communicate with epiviz app.
#' @field data_mgr An object of class \code{\link[epivizrData]{EpivizDataMgr}} used to serve data to epiviz app.
#' @field chart_mgr An object of class \code{EpivizChartMgr} used to manage charts added to epiviz app session.
#' 
#' @importClassesFrom epivizrServer EpivizServer
#' @importClassesFrom epivizrData EpivizDataMgr EpivizMeasurement EpivizData
#' @import GenomicRanges 
#' @import S4Vectors
#' @import methods
#' @import IRanges
#' 
#' @include EpivizChartMgr-class.R
#' @exportClass EpivizApp
EpivizApp <- setRefClass("EpivizApp",
  fields=list(
    .url_parms="list",
    .browser_fun="function",
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
  plot = function(data_object, datasource_name=NULL, send_request=TRUE, settings=NULL, colors=NULL, ...) {
    "Visualize data on epiviz app using its default chart type. Measurements from the \\code{data-object} 
    are first added to the epiviz app using the \\code{add_measurements} method for class 
    \\code{\\link[epivizrData]{EpivizData}}. See documentation for \\code{\\link[epivizrData]{register}}
    for information on supported data types and the \\code{\\link[epivizrData]{EpivizData}} class
    encapsulating this type of data. Once measurements are loaded, the \\code{\\link{plot}} method
    of class \\code{\\link{EpivizChartMgr}} is used to plot the data, using the default chart type
    for this type of data.
    
    \\describe{
      \\item{data_object}{An object to plot in epiviz app.}
      \\item{datasource_name}{Name to use for datasource, parses \\code{data_object} if missing or NULL}
      \\item{...}{Additional arguments passed to \\code{add_measurements} method for class
        \\code{\\link[epivizrData]{EpivizData}}}
    }"
    if (missing(datasource_name)) {
      datasource_name <- deparse(substitute(data_object))
    }
    ms_obj <- .self$data_mgr$add_measurements(data_object, 
                                              datasource_name=datasource_name,
                                              send_request=send_request, ...)
    .self$server$wait_to_clear_requests()
    
    if (send_request && .self$server$is_interactive() && !.self$data_mgr$is_ms_connected(ms_obj)) {
      stop("Error adding measurements for data object\n")
    }
    .self$chart_mgr$plot(ms_obj, send_request=send_request, settings=settings, colors=colors)
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
    .self$server$send_request(request_data, callback)
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
  .wait_for_connection = function(timeout=10L) {
    if (.self$server$.verbose) {
      cat("Servicing websocket until connected\n")
    }
    
    ptm <- proc.time()["elapsed"]
    while(!.self$server$is_socket_connected() && (proc.time()["elapsed"] - ptm  < timeout)) {
      .self$service(verbose=FALSE)
    }
    
    if (!.self$server$is_socket_connected()) {
      stop("[epivizr] Error opening connection. UI unable to connect to websocket server.")
    }
  },
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
      .self$.url_parms$ws_port <- .self$server$.port
      url <- do.call(.constructURL, .self$.url_parms)
      
      if(.self$server$.verbose) {
        cat("Using url ", url, "\n")
      }
      
      .self$.browser_fun(url)
      if (.self$server$is_daemonized()) {
        return(invisible())
      }
      .self$.wait_for_connection()      
    }, error=function(e) {
      if (close_on_error) .self$server$stop_server()
      stop(e)
    })
  },
  load_workspace=function(workspace_id=NULL) {
    "load an epiviz workspace.

    \\describe{
      \\item{workspace_id}{Workspace id to load from the database.}
    }"
    callback=function(data) {
      cat("workspace: ", workspace_id  ," is being loaded \n")
    }
    
    request_data <- list(action = "loadWorkspace", workspaceId=workspace_id)
    .self$server$send_request(request_data, callback)
    invisible()
  },
  print_workspace=function(file_name=NULL, file_type="pdf"){
    "Save epiviz workspace as a pdf or png."
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

# save method
EpivizApp$methods(
  save = function(file, stop_server=TRUE) {
    "Save EpivizApp object representation of a workspace into .RData file.

    \\describe{
      \\item{file}{(character) The name of the file to save the EpivizApp object into, ending in .rda.}
    }"
    if (!is(.self, "EpivizApp")) {
      stop("'app' must be an 'EpivizApp' object")
    }
    
    if (.self$is_server_closed()) {
      stop("The server for 'app' is closed")
    }
    loc <- NULL
    .self$get_current_location(function(response) {
      if (response$success) {
        loc <<- response$value
      }})
    .self$.url_parms$chr <- loc$seqName
    .self$.url_parms$start <- loc$start
    .self$.url_parms$end <- loc$end
    
    if (stop_server==TRUE) {
      .self$stop_app()
      .self$server$stop_server()
    }
    base::save(.self, file=file)
  }
)

  