EpivizDevice <- setRefClass("EpivizDevice",
	fields=list(
		msObject="EpivizData",
		chartObject="EpivizChart",
		id="character"
	),
	methods=list(
		getMsObject=function() {msObject},
		getChartObject=function() {chartObject},
		getMsId=function() {msObject$getId()},
		getChartId=function() {chartObject$getId()},
		setId=function(id) {id <<- id},
		getId=function() {id},
		update=function(newObject, sendRequest=TRUE) {
			msObject$update(newObject, sendRequest=sendRequest)
                    },
            set=function(...) {chartObject$set(...)}
	)
)
