EpivizChart <- setRefClass("EpivizChart",
	fields=list(
		.measurements = "list",
    .datasource = "ANY",
    .datasourceGroup = "ANY",
		.id = "character",
		.mgr = "EpivizChartMgr",
		.type="character"),
	methods=list(
		set_id = function(id) { .self$.id <- id },
		get_id = function() { .self$.id },
		show = function() {
			cat("EpivizChart object: ", .self$get_id(), "\n")
			cat("type: ", .self$.type, "\n")
      nms <- sapply(.self$.measurements, function(x) paste0(x$datasourceId, ":", x$id))
			cat("measurements: ", paste0(nms, collapse=","), "\n")
		}
	)
)

