#' Class providing chart manager for epiviz app
#' 
#' @import methods
#' @importClassesFrom epivizrServer EpivizServer
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
    num_charts= function() { 
      "Return the number of charts currently loaded through manager."
      length(ls(.self$.chart_list)) 
    },
    show = function() {
      "Print manager information to screen."
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
      "Returns \\code{TRUE} if underlying server is closed.
      See \\code{is_closed} method in class \\code{\\link[epivizrServer]{EpivizServer}}."
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
      "Add a chart to the chart manager.
       \\describe{
        \\item{chart_object}{an object of class \\code{\\link{EpivizChart}}}
        \\item{send_request}{send request to app through websocket}
      }"
      .self$.chart_id_counter <- .self$.chart_id_counter + 1L
      chart_id <- sprintf("epivizChart_%d", .self$.chart_id_counter)
      chart_object$set_id(chart_id)
      .self$.chart_list[[chart_id]] <- chart_object

      send_request <- !.self$is_server_closed() && isTRUE(send_request)      
      if (send_request) {
        callback <- function(response_data) {
          app_chart_id <- response_data$value$id
          chart_object$set_app_id(app_chart_id)
          cat("Chart ", chart_id, " added to browser and connected to id ", app_chart_id, "\n")
        }
        measurements <- NULL
        if (!is.null(chart_object$.measurements)) { 
          measurements = epivizrServer::json_writer(lapply(chart_object$.measurements, epivizrData::as.list)) 
        }
        
        request_data=list(action="addChart",
          type=chart_object$.type,
          measurements=measurements,
          datasource=chart_object$.datasource,
          datasourceGroup=chart_object$.datasourceGroup
        )
        .self$.server$send_request(request_data, callback)
      }
      invisible()
    },
    print_chart=function(chart_object_or_id, file_name=NULL, file_type="pdf") {
      "Remove chart from chart manager.
       \\describe{
        \\item{chart_object_or_id}{An object of class \\code{\\link{EpivizChart}} or a 
          string indicating the chart's id assigned by chart manager}
       }"
      chart_object <- .self$.get_chart_object(chart_object_or_id)
      
      if (!is(chart_object, "EpivizChart"))
        stop("'chartObj' must be an 'EpivizChart' object")
      
      chart_id <- chart_object$get_id()
      if(!exists(chart_id, envir=.self$.chart_list, inherits=FALSE)) {
        stop("object not found")
      }
      
      if (chart_object$is_connected()) {
        callback <- function(response) {
          cat("chart ", chart_id, " is being saved\n")
        }
        
        request_data <- list(action = "printWorkspace",
                             chartId = chart_object$.app_id,
                             fileName = file_name,
                             fileType = file_type)
        .self$.server$send_request(request_data, callback)
      }
      invisible()
    },
    rm_chart = function(chart_object_or_id) {
      "Remove chart from chart manager.
       \\describe{
        \\item{chart_object_or_id}{An object of class \\code{\\link{EpivizChart}} or a 
          string indicating the chart's id assigned by chart manager}
       }"
      chart_object <- .self$.get_chart_object(chart_object_or_id)

      if (!is(chart_object, "EpivizChart"))
        stop("'chart_object' must be an 'EpivizChart' object")
      
      chart_id <- chart_object$get_id()
      if(!exists(chart_id, envir=.self$.chart_list, inherits=FALSE)) {
        stop("object not found")
      }
      rm(list=chart_id, envir=.self$.chart_list)

      if (chart_object$is_connected()) {
        callback <- function(response) {
          cat("chart ", chart_id, " removed and disconnected\n")
        }
        
        request_data <- list(action = "removeChart",
          chartId = chart_object$.app_id)
        .self$.server$send_request(request_data, callback)
      }
      invisible()
    },
    rm_all_charts = function() {
      "Remove all charts loaded by chart manager."
      ids <- ls(.self$.chart_list)
      for (id in ids) {
          .self$rm_chart(id)
      }
      invisible()
    },
    list_charts = function() {
      "Return \\code{data.frame} describing charts loaded by chart manager"
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
    register_chart_type = function(chart_type, js_chart_type=paste0("epiviz.plugins.charts.", chart_type), js_chart_settings=NULL, js_chart_colors=NULL) {
      "Register a chart type name to a JavaScript chart type in the epiviz app.
      \\describe{
        \\item{chart_type}{the name to use for chart type in R (e.g., 'BlocksTrack')}
        \\item{js_chart_type}{the full JavaScript class name of the corresponding chart type 
          (e.g. 'epiviz.plugins.charts.BlocksTrack'). If missing it is taken from the \\code{chart_type} argument}
        \\item{js_chart_settings}{custom settings that can be applied to charts in JS}
        \\item{js_chart_colors}{default color palette applied to charts in JS}
      }"
      .self$.chart_type_map[[chart_type]] <- list(js_chart_type=js_chart_type, js_chart_settings=js_chart_settings, js_chart_colors=js_chart_colors)
    },
    list_available_chart_type = function(chart_type) {
      "List available options for a chart type.
      \\describe{
        \\item{chart_type}{the name for a chart type in R (e.g., 'BlocksTrack', 'StackedLineTrack')}
      }"
      chart_settings <- .self$.chart_type_map[[chart_type]]$js_chart_settings
      chart_colors <- .self$.chart_type_map[[chart_type]]$js_chart_colors
      
      cat("\n chart settings for ", chart_type, "\n")
      .self$.print_chart_settings(chart_settings, chart_colors)
      
    },
    list_all_available_chart_types = function() {
      "Print charts types with their default settings and colors registered with epivizr"
        
      types <- ls(.self$.chart_type_map)
      
      for (type in types) {
        .self$list_available_chart_type(type)
      }
      invisible()
    },
    register_available_chart_types = function() {
      "request the epiviz app to get available chart types and registers them 
      with the epivizr session."
      callback <- function(response_data) {
        data <- response_data$value
        register_chartType <- function(x) {
          .self$register_chart_type(gsub("epiviz.plugins.charts.", "", x$chartName, fixed = TRUE), 
                                    js_chart_type=x$chartName, 
                                    js_chart_settings=x$customSettings,
                                    js_chart_colors=x$colorMap)
        }
        out <- sapply(data, register_chartType)
      }
      
      request_data=list(action="getAvailableCharts")
      .self$.server$send_request(request_data, callback)
    },
    .print_chart_settings = function(chart_settings, chart_colors) {
      ids <- sapply(chart_settings, function(x) x$id)
      labels <- sapply(chart_settings, function(x) x$label)
      values <- sapply(chart_settings, function(x) x$defaultValue)
      poss_values <- sapply(chart_settings, function(x) {
        paste0(x$possibleValues, collapse=",")
      })
      types <- sapply(chart_settings, function(x) x$type)
      
      colors <- paste0(chart_colors, collapse=",")
      
      out <- data.frame(ids=ids,
                        labels=labels,
                        default_values=values,
                        possible_values=poss_values,
                        types=types,
                        stringsAsFactors=FALSE)
      # rownames(out) <- NULL
      print(out)
      
      cat("chart colors - ", colors, "\n\n")
    },
    .map_chart_settings = function(js_chart_settings, settings=NULL) {
      if(!is.null(settings)) {
        
        setParams <- settings
        
        out_settings <- lapply(js_chart_settings, function(val) {
          if(exists(val$id, settings)) {
            set <- val$id
            val$defaultValue <- settings[[set]]
            setParams[[set]] <<- NULL
          }
          val
        })
        
        # list unrecognized settings
        if(length(setParams) > 0) {
          warning("unrecognized params - (", paste0(names(setParams), collapse=","), ") discarded! \n")
          cat("Please use the method: list_available_chart_types for available settings! \n")
        }
        
      }
      else {
        out_settings <- js_chart_settings
      }
      
      out_settings
    },
    set_chart_settings = function(chart_object_or_id, settings=NULL, colors=NULL) {
      "Apply custom chart settings or colors to a chart object
      
      \\describe{
        \\item{chart_object_or_id}{An object of class \\code{\\link{EpivizChart}} or a 
          string indicating the chart's id assigned by chart manager}
        \\item{settings}{a list of settings to apply to the chart object}
        \\item{colors}{a list of colors to use as default colors for the chart object}
      }"
      chart_object <- .self$.get_chart_object(chart_object_or_id)
      
      if (!is(chart_object, "EpivizChart"))
        stop("'chart_object' must be an 'EpivizChart' object")
      
      chart_id <- chart_object$get_id()
      if(!exists(chart_id, envir=.self$.chart_list, inherits=FALSE)) {
        stop("object not found")
      }
      
      chart_settings = chart_object$get_settings()
      
      if(!is.null(settings)) {
        chart_settings <- .self$.map_chart_settings(chart_settings, settings)
      }
      
      chart_colors = chart_object$get_colors()
      
      if(!is.null(colors)) {
        chart_colors <- colors
      }
      
      if (chart_object$is_connected()) {
        callback <- function(response) {
          cat("settings successfully applied to ", chart_id)
        }
        
        request_data <- list(action = "setChartSettings",
                             chartId = chart_object$.app_id,
                             settings = chart_settings,
                             colorMap = chart_colors)
        .self$.server$send_request(request_data, callback)
      }
      invisible()
      
    },
    get_chart_settings = function(chart_object_or_id) {
      "List custom chart settings from a chart object
      \\describe{
      \\item{chart_object_or_id}{An object of class \\code{\\link{EpivizChart}} or a 
      string indicating the chart's id assigned by chart manager}
      }"
      chart_object <- .self$.get_chart_object(chart_object_or_id)
      
      if (!is(chart_object, "EpivizChart"))
        stop("'chart_object' must be an 'EpivizChart' object")
      
      chart_id <- chart_object$get_id()
      if(!exists(chart_id, envir=.self$.chart_list, inherits=FALSE)) {
        stop("object not found")
      }
      
      chart_settings = chart_object$get_settings()
      chart_colors = chart_object$get_colors()
      
      cat("\n chart settings for ", chart_id, "\n")
      
      .self$.print_chart_settings(chart_settings, chart_colors)
      invisible()
      },
    visualize = function(chart_type, measurements = NULL, datasource = NULL, send_request=TRUE, settings=NULL, colors=NULL, ...) {
      "Visualize data use the given chart type. One of arguments \\code{measurements} or \\code{datasource} must be non-\\code{NULL}. If \\code{measurements}
      is \\code{NULL}, the \\code{get_measurements} method in class \\code{\\link[epivizrData]{EpivizData}}
      is used to decide which measurements are used in the chart
      
      \\describe{
        \\item{chart_type}{a chart type registered using the \\code{register_chart_type} method}
        \\item{measurements}{a list of \\code{\\link[epivizrData]{EpivizMeasurement}} objects
          describing measurements to include in the chart}
        \\item{datasource}{an object of class \\code{\\link[epivizrData]{EpivizData}}, all available
          measurements from datasource are used as appropriate}
      }"
      
      js_chart_type <- .self$.chart_type_map[[chart_type]]$js_chart_type
      js_chart_settings <- .self$.chart_type_map[[chart_type]]$js_chart_settings
      js_chart_colors <- .self$.chart_type_map[[chart_type]]$js_chart_colors
      
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
      
      # out_settings <- .self$.map_settings(js_chart_settings, settings)
      
      # if(!is.null(colors)) {
      #   js_chart_colors <- colors
      # }
      
      chart_obj <- EpivizChart$new(
        .measurements=measurements,
        .datasource=datasource_id,
        .datasourceGroup=datasource_id,
        .mgr=.self,
        .type=js_chart_type,
        .settings=js_chart_settings,
        .colors=js_chart_colors)
      .self$add_chart(chart_obj, send_request=send_request, ...)
      .self$.server$wait_to_clear_requests()
      
      # if (!chart_obj$is_connected() && !.self$is_server_closed()) {
      #   stop("Error adding chart to epiviz\n")
      # }
      
      chart_id <- chart_obj$get_id()
      if(!exists(chart_id, envir=.self$.chart_list, inherits=FALSE)) {
        stop("Chart object not found \n")
      }
      
      .self$set_chart_settings(chart_obj, settings=settings, colors=colors)
      chart_obj
    },
    plot = function(measurement_object, send_request=TRUE, settings=NULL, colors=NULL) {
      "Visualize data in an \\code{\\link[epivizrData]{EpivizData}} object using its default chart type.
      The method \\code{get_default_chart_type} in class \\code{\\link[epivizrData]{EpivizData}} is used
      to determine which chart type is used.

      \\describe{
        \\item{measurement_object}{an object of class \\code{\\link[epivizrData]{EpivizData}}}
      }"
      if (!is(measurement_object, "EpivizData")) {
        stop("'measurement_object' must be of class 'EpivizData'")
      }
      chart_type <- measurement_object$get_default_chart_type()
      .self$visualize(chart_type, datasource=measurement_object, send_request=send_request, settings=settings, colors=colors)
    },
    .redraw = function(send_request = TRUE) {
      send_request <- !.self$is_server_closed() && isTRUE(send_request)      
      if (send_request) {
        callback <- function(response_data) {
          if (.self$.server$.verbose) {
            cat("charts redraw\n")            
          }
        }
        request_data <- list(action="redraw")
        .self$.server$send_request(request_data, callback)
      }
      invisible()
    }
  )
)                              