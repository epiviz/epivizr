EpivizChart <- setRefClass("EpivizChart",
	fields=list(
		measurements="list",
		id="character",
		mgr="EpivizDeviceMgr",
		inDevice="logical",
		type="character"),
	methods=list(
		initialize=function(inDevice=FALSE, ...) {
			inDevice <<- inDevice
			callSuper(...)
		},
		setId=function(id) {id <<- id},
		getId=function() {return(id)},
		setInDevice=function(x) {inDevice <<- x},
		show=function() {
			cat("EpivizChart object: ", getId(), "\n")
			cat("type: ", type, "\n")
                        nms <- sapply(measurements, function(x) paste0(x$id,"__",x$datasourceId))
			cat("measurements: ", paste0(nms, collapse=","), "\n")
		}
	)
)

