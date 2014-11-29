EpivizChart <- setRefClass("EpivizChart",
	fields=list(
		measurements="list",
		id="character",
		mgr="EpivizDeviceMgr",
		inDevice="logical",
            type="character",
            settings="EpivizChartSettings"
        ),
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
                        nms <- sapply(measurements, function(x) paste0(x$datasourceId,":",x$id))
			cat("measurements: ", paste0(nms, collapse=","), "\n")
               },
            set=function(...) {
                args = list(...)
                if (length(args) == 0) {
                    out <- mgr$getChartSettings(id)
                    if (!settings$initialized) {
                        epivizrMsg("error getting chart settings for chart", id)
                        return(NULL)
                    }
                    return(out)
                    
                } else {
                    if (!settings$initialized) {
                        mgr$getChartSettings(id)
                    }
                    if (!settings$initialized) {
                        epivizrMsg("error getting chart settings for chart", id)
                        return(NULL)
                    }

                    res <- settings$set(args)
                    if (is.null(res)) {
                        return(NULL)
                    }
                    mgr$setChartSettings(id, res$settings, res$colorMap)
                }
            }
	)
)

