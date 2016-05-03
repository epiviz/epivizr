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
          
          c_ids <- ls(.self$.chart_list)
          if (length(c_ids) == 0) {
            return(NULL)
          }
          
          match_id <- NULL
          
          for (c_id in c_ids) {
            if(.self$.chart_list[[c_id]]$.app_id == id) {
              match_id <- c_id
            }
          }
          
          if(is.null(match_id)) {
            stop("chart with id ", id, " not found")  
          }
          
          id <- match_id
          
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
    print_chart=function(chart_object_or_id, file_name=NULL, file_type=c("pdf","png")) {
      "Print specific chart to file.
       \\describe{
        \\item{chart_object_or_id}{An object of class \\code{\\link{EpivizChart}} or a 
          string indicating the chart's id assigned by chart manager.}
        \\item{file_name}{Name of file to print to.}
        \\item{file_type}{Type of file to print. One of \\code{pdf} or \\code{png}.}
       }"
      chart_object <- .self$.get_chart_object(chart_object_or_id)
      
      if (!is(chart_object, "EpivizChart"))
        stop("'chartObj' must be an 'EpivizChart' object")
      
      chart_id <- chart_object$get_id()
      if(!exists(chart_id, envir=.self$.chart_list, inherits=FALSE)) {
        stop("chart ", chart_id, " not found")
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
    register_chart_type = function(chart_type, js_chart_type=paste0("epiviz.plugins.charts.", chart_type), 
                                   js_chart_settings=NULL, js_chart_colors=NULL) {
      "Register a chart type name to a JavaScript chart type in the epiviz app.
      \\describe{
        \\item{chart_type}{the name to use for chart type in R (e.g., 'BlocksTrack')}
        \\item{js_chart_type}{the full JavaScript class name of the corresponding chart type 
          (e.g. 'epiviz.plugins.charts.BlocksTrack'). If missing it is taken from the \\code{chart_type} argument}
        \\item{js_chart_settings}{custom settings that can be applied to this chart type in JS.}
        \\item{js_chart_colors}{default color palette applied to this chart type in JS.}
      }"
      .self$.chart_type_map[[chart_type]] <- list(js_chart_type=js_chart_type, js_chart_settings=js_chart_settings, js_chart_colors=js_chart_colors)
    },
    print_chart_type_info = function(chart_type) {
      "Print settings and color information for a given chart type
      \\describe{
        \\item{chart_type}{the name for a chart type in R (e.g., 'BlocksTrack', 'StackedLineTrack')}
      }"
      cat("Settings for chart type ", chart_type, "\n")
      print(.self$list_chart_type_settings(chart_type))
      
      cat("Colors: ")
      cat(paste0(.self$list_chart_type_colors(chart_type), collapse=", "), "\n")
    },
    list_chart_type_settings = function(chart_type) {
      "List available settings for a specific chart type.
      \\describe{
        \\item{chart_type}{the name for a chart type in R (e.g., 'BlocksTrack', 'StackedLineTrack')}
      }"
      chart_settings <- .self$.chart_type_map[[chart_type]]$js_chart_settings
      .settings_as_df(chart_settings)
    },
    list_chart_type_colors = function(chart_type) {
      "List colors currently used in given chart type"
      chart_colors <- .self$.chart_type_map[[chart_type]]$js_chart_colors
      chart_colors
    },
    list_chart_types = function(col_width=80) {
      "List charts types registered in epivizr with their default settings and colors. 
       Returns a \\code{data.frame} listing available chart types and a summary of the 
       settings that can be modified.
      
      \\describe{
        \\item{col_width}{Maximum length of settings list displayed.}
      }"
      types <- ls(.self$.chart_type_map)
      js_classes <- sapply(types, function(chart_type) {
        .self$.chart_type_map[[chart_type]]$js_chart_type  
      })
      
      num_settings <- sapply(types, function(chart_type) {
        length(.self$.chart_type_map[[chart_type]]$js_chart_settings)
      })
      
      settings_prefix <- sapply(types, function(chart_type) {
        setting_ids <- sapply(.self$.chart_type_map[[chart_type]]$js_chart_settings, function(x) x$id)
        out <- paste0(setting_ids, collapse=",")
        if (nchar(out) > col_width) {
          out <- paste0(substr(out, 1, col_width), "...")
        }
        out
      })
      num_colors <- sapply(types, function(chart_type) {
        length(.self$.chart_type_map[[chart_type]]$js_chart_colors)
      })
      out <- data.frame(type=types, js_class=js_classes, num_settings=num_settings, 
                 settings=settings_prefix, num_colors=num_colors)
      rownames(out) <- NULL
      out
    },
    .register_available_chart_types = function(request_data) {
      for (x in request_data) {
        .self$register_chart_type(gsub("epiviz.plugins.charts.", "", x$chartName, fixed = TRUE), 
                                  js_chart_type=x$chartName, 
                                  js_chart_settings=x$customSettings,
                                  js_chart_colors=x$colorMap)
      }
      invisible(NULL)
    },
    .update_chart_settings = function(request_data) {
      
      chart_app_id <- request_data$chartId
      chart_object <- .self$.get_chart_object(chart_app_id)
      
      if (!is(chart_object, "EpivizChart"))
        warning("'chart_object' must be an 'EpivizChart' object")
      
      chart_id <- chart_object$get_id()
      if(!exists(chart_id, envir=.self$.chart_list, inherits=FALSE)) {
        warning("object ", chart_id, " not found")
      }
      
      chart_object$.update(settings=request_data$settings, colors=request_data$colorMap)
      invisible()
      
    },
    set_chart_settings = function(chart_object_or_id, settings=NULL, colors=NULL) {
      "Apply custom chart settings or colors to a chart object.
      
      \\describe{
        \\item{chart_object_or_id}{An object of class \\code{\\link{EpivizChart}} or a 
          string indicating the chart's id assigned by chart manager}
        \\item{settings}{a list of settings to apply to the chart}
        \\item{colors}{a list of (HEX code) colors to use in the chart}
      }"
      chart_object <- .self$.get_chart_object(chart_object_or_id)
      
      if (!is(chart_object, "EpivizChart"))
        stop("'chart_object' must be an 'EpivizChart' object")
      
      chart_id <- chart_object$get_id()
      if(!exists(chart_id, envir=.self$.chart_list, inherits=FALSE)) {
        stop("object ", chart_id, " not found")
      }
      
      chart_object$set(settings=settings, colors=colors)
      invisible()
      
    },
    get_chart_settings = function(chart_object_or_id) {
      "List chart settings for a specific chart object.
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
      
      chart_object$.get_chart_settings_df()
    },
    get_chart_colors = function(chart_object_or_id) {
      "List colors used in a specific chart object.
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
      
      chart_object$get_colors()
    },
    print_chart_info = function(chart_object_or_id) {
      "Print settings and colors used in a specific chart object.
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
      
      chart_object$print_info()
      invisible()
    },
    visualize = function(chart_type, measurements = NULL, datasource = NULL, 
                         settings=NULL, colors=NULL, send_request=TRUE, ...) {
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
      
      if (!is.null(settings) || !is.null(colors)) {
        .self$set_chart_settings(chart_obj, settings=settings, colors=colors)
      }
      chart_obj
    },
    plot = function(measurement_object, settings=NULL, colors=NULL, send_request=TRUE) {
      "Visualize data in an \\code{\\link[epivizrData]{EpivizData}} object using its default chart type.
      The method \\code{get_default_chart_type} in class \\code{\\link[epivizrData]{EpivizData}} is used
      to determine which chart type is used.

      \\describe{
        \\item{measurement_object}{an object of class \\code{\\link[epivizrData]{EpivizData}}}
        \\item{settings}{list of settings to use in chart (uses default chart settings if NULL)}
        \\item{colors}{character vector of HEX colors to use in chart (uses default chart colors if NULL)}
      }"
      if (!is(measurement_object, "EpivizData")) {
        stop("'measurement_object' must be of class 'EpivizData'")
      }
      chart_type <- measurement_object$get_default_chart_type()
      .self$visualize(chart_type, datasource=measurement_object, settings=settings, colors=colors, send_request=send_request)
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