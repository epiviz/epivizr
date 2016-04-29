#' Class encapsulating a chart in epiviz app
#' 
#' @import methods
#' @include EpivizChartMgr-class.R
EpivizChart <- setRefClass("EpivizChart",
	fields=list(
		.measurements = "list",
    .datasource = "ANY",
    .datasourceGroup = "ANY",
		.mgr_id = "character",
		.app_id = "character",
		.mgr = "EpivizChartMgr",
		.type ="character",
		.settings = "list",
		.colors = "character" ),
	methods=list(
		set_id = function(id) { 
		  "Set chart's id assigned by chart manager."
		  .self$.mgr_id <- id 
		},
		get_id = function() { 
		  "Get chart's id assigned by chart manager"
		  .self$.mgr_id 
		},
		set_app_id = function(id) { 
		  "Set chart's id assigned by epiviz app."
		  .self$.app_id <- id 
		},
		get_app_id = function(id) { 
		  "Get chart's id assigned by epiviz app."
		  .self$.app_id 
		},
		is_connected = function() { 
		  "Returns \\code{TRUE} if chart is connected to a chart on epiviz app."
		  isTRUE(length(.self$.app_id) > 0) 
		},
		show = function() {
		  "Print information about chart."
			cat("EpivizChart object ", .self$get_id(), ":\n")
			cat("type: ", .self$.type, "\n")
      nms <- sapply(.self$.measurements, function(x) paste0(x@datasourceId, ":", x@id))
			cat("measurements: ", paste0(nms, collapse=","), "\n")
		},
		print_info = function() {
		  "Print settings and color currently used in chart object."
		  cat("Chart settings for chart id ", .self$.mgr_id, ":\n")
		  print(.settings_as_df(.self$.settings))
		  
		  cat("Colors: ")
		  cat(paste0(.self$.colors, collapse=", "), "\n")
		  invisible()
		},
		set = function(settings=NULL, colors=NULL) {
		  "Set settings and colors used in chart.
		  \\describe{
		    \\item{settings}{List of settings to apply.}
        \\item{colors}{Character vector of HEX colors to use in chart.}
		  }"
		  if (!is.null(settings)) {
  		  settings <- .self$set_settings(settings)
		  }
		  
		  if (!is.null(colors)) {
  		  .self$set_colors(colors)
		  }
		  
		  if (.self$is_connected() && !(is.null(settings) && is.null(colors))) {
		    callback <- function(response) {
		      cat("settings successfully applied to ", .self$.mgr_id)
		    }
		    
		    request_data <- list(action = "setChartSettings",
		                         chartId = .self$.app_id,
		                         settings = settings,
		                         colorMap = .self$get_colors())
		    .self$.mgr$.server$send_request(request_data, callback)
		  }
		  invisible()
		},
		.filter_settings = function(settings=NULL) {
		  if (is.null(settings)) {
		    return(settings)
		  }
		  
		  js_chart_settings <- .self$.settings
		  is_setting_valid <- names(settings) %in% sapply(js_chart_settings, function(x) x$id)
		  invalid_settings <- names(settings)[!is_setting_valid]
		  
		  # list unrecognized settings
		  if(sum(!is_setting_valid) > 0) {
		    warning("unrecognized settings - (", paste0(invalid_settings, collapse=","), ") discarded! \n")
		    cat("Please use the method: list_chart_settings for available settings! \n")
		  }
		  
		  if (sum(is_setting_valid) == 0) {
		    return(NULL)
		  } 
		  
		  out_settings <- settings[is_setting_valid]
		  out_settings
		},
		set_settings = function(settings) {
		  "Set custom settings for the chart. Returns subset of valid settings.
		  \\describe{
		    \\item{settings}{List of settings to apply.}
		  }"
		  if (!is.null(settings)) {
		    settings <- .self$.filter_settings(settings)
		    for (id in names(settings)) {
		      .self$.settings[[id]] <- settings[[id]]
		    }
		  }
		  settings
		},
		get_settings = function() {
		  "Get custom settings for the chart."
		  .self$.settings
		},
		set_colors = function(colors) {
		  "Set custom colors for the chart."
		  .self$.colors <- colors
		},
		get_colors = function() {
		  "Get color palette applied to the chart."
		  .self$.colors
		}
	)
)

