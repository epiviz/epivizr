EpivizWigData <- setRefClass("EpivizWigData",
  contains="EpivizBpData",
  fields=list(cache="EpivizWigCache", file="BigWigFile", indexOffset="integer"),                            
  methods=list(
    initialize=function(object=GIntervalTree(GRanges(score=numeric())),
        file=BigWigFile(),
        windowSize = 0L,
        indexOffset=0L, ...) {
      file <<- file
      cache <<- EpivizWigCache(resource=file, windowSize=windowSize, ...)
      indexOffset <<- indexOffset
      
      callSuper(object=object, columns="score", ...)
    },
    .getLimits=function() {
      .checkSumm <- function(summ) {
        !is(summ, "list") ||
        !all(sapply(summ, is, "GenomicRanges")) ||
        !all(sapply(minSum, function(x) "score" %in% names(mcols(x))))
      }              
      minSumm <- summary(file, type="min")
      if (!.checkSumm(minSumm)) {
        minScore <- -6
      } else {
        minScore <- min(sapply(minSumm, function(x) x$score))
      }
      maxSumm <- summary(file, type="max")
      if (!.checkSumm(maxSumm)) {
        maxScore <- 6
      }  else {
        maxScore <- max(sapply(maxSumm, function(x) x$score))
      }
      cbind(c(minScore,maxScore))    
    },
    getRows=function(query, metadata, useOffset=TRUE) {
      tmp <- cache$getObject(query)
      if (!is.null(tmp)) {
          newObject <- tmp[[1]]
          action <- tmp[[2]]
          
          if (action == "replace") {
              update(newObject)
              indexOffset <<- 0L
          }
          else if (action == "right") {
              object <<- GIntervalTree(c(as(object, "GRanges"), newObject))
          } else {
              indexOffset <<- as.integer(length(newObject) + 1)
              object <<- GIntervalTree(c(newObject, as(object, "GRanges")))
          }
      }
      res <- callSuper(query, metadata, useOffset=useOffset)
      if (indexOffset != 0 && !is.null(res$globalStartIndex)) {
          res$globalStartIndex <- res$globalStartIndex - indexOffset
          if (!is.list(res$values$id)) {
              res$values$id <- res$values$id - indexOffset
          } else {
              res$values$id <- list(res$values$id[[1]] - indexOffset)
          }
      }
      return(res)
    },
    .getMetadata=function(curHits, curMetadata) {
      return(NULL)
    },
    .getValues=function(curHits, measurement, round=TRUE) {
      callSuper(curHits, measurement, round=round)
    },
    plot=function(...) {
      ms <- getMeasurements()
      mgr$lineChart(ms, ...)
    }
 )
)                             
