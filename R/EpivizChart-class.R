#' Class encapsulating a chart in epiviz app
#' 
#' @import methods
#' @exportClass EpivizChart
EpivizChart <- setRefClass("EpivizChart",
	fields=list(
		.measurements = "list",
    .datasource = "ANY",
    .datasourceGroup = "ANY",
	  .datasourceOriginName = "character",
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
	  get_source_name = function() { 
	    "Get chart's id assigned by chart manager"
	    .self$.datasourceOriginName
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
		print_info = function() {
		  "Print settings and color currently used in chart object."
		  cat("Chart settings for chart id ", .self$.mgr_id, ":\n")
		  print(.settings_as_df(.self$.settings))
		  
		  cat("Colors: ")
		  cat(paste0(.self$.colors, collapse=", "), "\n")
		  invisible()
		},
		.update = function(settings=NULL, colors=NULL) {
		  if (!is.null(settings)) {
		    settings <- .self$set_settings(settings)
		  }
		  
		  if (!is.null(colors)) {
		    .self$set_colors(colors)
		  }
		  
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
		      pos <- which(sapply(.self$.settings, function(x) x$id) == id)
		      .self$.settings[[pos]]$defaultValue <- settings[[id]]
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
    },
		get_measurements = function() {
  	  "Get measurements for the chart."
  	  .self$.measurements
    }
	)
)

#' Print information about EpivizChart
#' @param object an \code{\link{EpivizChart}} object
#' @return Nothing, this function is called for its side-effects
#' @export
setMethod("show", signature(object="EpivizChart"),
  function(object) {
    cat("EpivizChart object ", object$get_id(), ":\n")
    cat("type: ", object$.type, "\n")
    cat("measurements:\n")
    num_measurements <- length(object$.measurements)
    num_to_show <- pmin(10, num_measurements)
    
    for (i in seq_len(num_to_show)) {
      ms <- object$.measurements[[i]]
      cat(paste0("[", i, "] "))
      callGeneric(ms)
      cat("\n")
    }
    
    if (num_to_show < num_measurements) {
      num_left <- num_measurements - num_to_show
      cat(paste0("<", num_left, " more>"), "\n")
    }
})
