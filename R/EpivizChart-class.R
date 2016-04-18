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
		.type="character"),
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
		}
	)
)

