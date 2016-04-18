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
    .chart_type_map = "list"
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
    .get_chart_object = function(chart_object_or_id) {
      chart_obj <- NULL
      if (is.character(chart_object_or_id)) {
        id <- chart_object_or_id
        if (!exists(id, envir=.self$.chart_list, inherits=FALSE)) {
          stop("chart with id ", id, " not found")                  
        }
        chart_obj <- .self$.chart_list[[id]]
      } else {
        chart_obj <- chart_object_or_id
      }
      chart_obj
    },
    add_chart = function(chart_object, send_request=TRUE) {
      .self$.chart_id_counter <- .self$.chart_id_counter + 1L
      chart_id <- sprintf("epivizChart_%d", .self$.chart_id_counter)
      chart_object$set_id(chart_id)
      .self$.chart_list[[chart_id]] <- chart_object

      send_request <- !.self$is_server_closed() && isTRUE(send_request)      
      if (send_request) {
        callback <- function(response_data) {
          app_chart_id <- response_data$value$id
          chart_obj$set_app_id(app_chart_id)
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
    rm_chart = function(chart_object_or_id) {
      chart_object <- .self$.get_chart_object(chart_object_or_id)

      if (!is(chart_object, "EpivizChart"))
        stop("'chart_object' must be an 'EpivizChart' object")
      
      chart_id <- chart_object$get_id()
      if(!exists(chart_id, envir=.self$.chart_list, inherits=FALSE)) {
        stop("object not found")
      }
      rm(list=chart_id, envir=.self$.chart_list)

      chart_app_id <- chart_object$get_app_id()
      if(isTRUE(!is.null(chart_app_id) && (chart_app_id != character()))) {
        callback <- function(response) {
          cat("chart ", chart_id, " removed and disconnected\n")
        }
        
        request_data <- list(action = "removeChart",
          chartId = chart_app_id)
        .self$.server$send_request(request_data)
      }
      invisible()
    },
    rm_all_charts = function() {
      ids <- ls(.self$.chart_list)
      for (id in ids) {
          .self$rm_chart(id)
      }
      invisible()
    },
    list_charts = function() {
      ids <- ls(.self$.chart_list)
      if (length(ids) == 0) {
        return(NULL)
      }
      
      type <- sapply(ids, function(x) .self$.chart_list[[x]]$.type)
      ms <- sapply(ids,
                   function(x) {
                     tmp <- sapply(.self$.chart_list[[x]]$.measurements, function(y) paste0(y@datasourceId,":",y@name))
                     paste0(tmp, collapse=",")
                   })
      connected <- ifelse(sapply(ids, function(x) .self$.chart_list[[x]]$is_connected()), "*", "")
      out <- data.frame(id=ids,
                        type=type,
                        measurements=ms,
                        connected=connected,
                        stringsAsFactors=FALSE)
      rownames(out) <- NULL
      out
    },
    register_chart_type = function(chart_type, js_chart_type) {
      .self$.chart_type_map[[chart_type]] <- js_chart_type
    },
    visualize = function(chart_type, measurements = NULL, datasource = NULL, ...) {
      js_chart_type <- .self$.chart_type_map[[chart_type]]
      if (is.null(js_chart_type)) {
        stop("Can't visualize ", chart_type, ", it is not registered")
      }

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
        .type=js_chart_type)
      .self$add_chart(chart_obj, ...)
      chart_obj
    },
    plot = function(measurement_object) {
      if (!is(measurement_object, "EpivizData")) {
        stop("'measurement_object' must be of class 'EpivizData'")
      }
      chart_type <- measurement_object$get_default_chart_type()
      .self$visualize(chart_type, datasource=measurement_object)
    }
  )
)                              