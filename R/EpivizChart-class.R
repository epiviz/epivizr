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
		set_id = function(id) { .self$.mgr_id <- id },
		get_id = function() { .self$.mgr_id },
		set_app_id = function(id) { .self$.app_id <- id },
		get_app_id = function(id) { .self$.app_id },
		is_connected = function() { isTRUE(.self$.app_id != character()) },
		show = function() {
			cat("EpivizChart object: ", .self$get_id(), "\n")
			cat("type: ", .self$.type, "\n")
      nms <- sapply(.self$.measurements, function(x) paste0(x$datasourceId, ":", x$id))
			cat("measurements: ", paste0(nms, collapse=","), "\n")
		}
	)
)

