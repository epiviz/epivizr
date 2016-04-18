#' Class providing chart manager for epiviz app
#' 
#' @docType class
#' @import methods
#' @importClassFrom epivizrServer EpivizServer
EpivizChartMgr <- setRefClass("EpivizChartMgr",
  fields=list(
    .chart_list = "environment",
    .chart_id_counter = "integer",
    .server = "EpivizServer",
    .chart_type_map = "list",
    .chart_id_map = "list"
  ),
  methods=list(
    initialize = function(server=epivizrServer::createServer(), ...) {
      .self$.server <- server
      .self$.chart_list <- new.env(parent=emptyenv())
      .self$.chart_id_counter <- 0L
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
    add_chart = function(chart_object, send_request=TRUE) {
      .self$.chart_id_counter <- .self$.chart_id_counter + 1L
      chart_id <- sprintf("epivizChart_%d", .self$.chart_id_counter)
      chart_object$set_id(chart_id)
      .self$.chart_list[[chart_id]] <- chart_object

      send_request <- !.self$is_server_closed() && isTRUE(send_request)      
      if (send_request) {
        callback <- function(response_data) {
          app_chart_id = response_data$value$id
          .self$.chart_id_map[[chart_id]] <- app_chart_id
          cat("Chart ", chart_id, " added to browser and connected\n")
        }
        measurements <- NULL
        if (!is.null(chart_object$measurements)) { 
          measurements = rjson::json_writer(chart_object$measurements) 
        }
        
        request_data=list(action="addChart",
          type=chartObject$type,
          measurements=measurements,
          datasource=chartObject$datasource,
          datasourceGroup=chartObject$datasourceGroup
        )
        .self$.server$send_request(request_data, callback)
      }
      invisible()
    },
    rm_chart = function(chart_object_or_id) {},
    rm_all_charts = function() {},
    list_charts = function() {},
    register_chart_type = function(chart_type, js_chart_type) {
      .self$.chart_type_map[[chart_type]] <- js_chart_type
    },
    visualize = function(chart_type, measurements = NULL, datasource = NULL, ...) {
      if (is.null(measurements)) {
        if (!is.null(datasource)) {
          measurements = datasource$get_measurements()
        } else {
          stop("Either 'measurements' or 'datasource' must be non-NULL")
        }
      }
      datasource_id <- measurements$datasourceId

      chart_obj <- EpivizChart$new(
        .measurements=measurements,
        .datasource=datasource_id,
        .datasourceGroup=datasource_id,
        .mgr=.self,
        .type=.self$.chart_type_map[[chart_type]])
      .self$add_chart(chart_obj, ...)
      chart_obj
    }
  )
)                              