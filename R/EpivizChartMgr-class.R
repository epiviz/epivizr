#' Class providing chart manager for epiviz app
#' 
#' @docType class
#' @import methods
#' @importClassFrom epivizrServer EpivizServer
EpivizChartMgr <- setRefClass("EpivizChartMgr",
  fields=list(
    .chart_list = "environment",
    .chart_idCounter = "integer",
    .server = "EpivizServer"
  ),
  methods=list(
    initialize = function(server=epivizrServer::createServer(), ...) {
      .self$.server <- server
      .self$.chart_list <- new.env(parent=emptyenv())
      .self$.chart_idCounter <- 0L
    },
    num_charts= function() { length (ls(.self$.chart_list)) },
    show = function() {
      "Print manager information to screen"
      cat("Epiviz chart manager object:\n")
      cat("Server: ")
      .self$.server$show(); cat("\n")
#     st <- .self$list_charts()
#     if(length(st)>0) {
#        cat("Charts:\n")
#        print(st); cat("\n")
#      }      
    },
    is_server_closed = function() { 
      "Check if underlying server is closed, <logical>"
      is.null(.self$.server) || .self$.server$is_closed()
    },
    add_chart = function(chart_object, send_request=TRUE) {},
    rm_chart = function(chart_object_or_id) {},
    rm_all_charts = function() {},
    list_charts = function() {},
    visualize = function(chart_type, measurements = NULL, datasource = NULL, ...) {}
  )
)                              