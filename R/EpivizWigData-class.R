EpivizWigData <- setRefClass("EpivizWigData",
  contains="EpivizBpData",
  fields=list(caches="list", stashObjects="list",
      file="BigWigFile", indexOffset="integer",
      currentCache="integer", triggerValues="integer"),                            
  methods=list(
    initialize=function(object=GIntervalTree(GRanges(score=numeric())),
        file=BigWigFile(),
        windowSizes = c(0L, 1000L, 10000L),
        triggerValues = c(100L, 5000L), ...) {
      file <<- file
      indexOffset <<- 0L
      caches <<- lapply(windowSizes, function (size) EpivizWigCache(resource=file, windowSize=size, ...))
      stashObjects <<- lapply(windowSizes, function(size) list(obj=GIntervalTree(GRanges(score=numeric())), indexOffset=0L))
      currentCache <<- as.integer(pmin(length(caches), 2))
      triggerValues <<- triggerValues
      
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
    cache=function() caches[[currentCache]],
    switchCache=function(newCache) {
        if (mgr$verbose)
          epivizrMsg(id, "switching cache:", currentCache, "->", newCache)
        stashObjects[[currentCache]] <<- list(obj=object, indexOffset=indexOffset)
        tmp <- stashObjects[[newCache]]
        currentCache <<- newCache
        object <<- tmp$obj
        indexOffset <<- tmp$indexOffset
        return(invisible(NULL))
    },
    verifyCache=function(query) {
        currentWindowSize <- cache()$windowSize
        if (currentWindowSize > 0) {
            expectedResultSize <- ceiling(width(query) / currentWindowSize)
        } else {
            expectedResultSize <- width(query)
        }
        if (expectedResultSize < triggerValues[1] && currentCache > 1) {
            # increase cache resolution
            newCache <- currentCache - 1L
            switchCache(newCache)
            return(invisible(NULL))
        } else if (expectedResultSize > triggerValues[2] && currentCache < length(caches)) {
            newCache <- currentCache + 1L
            switchCache(newCache)
            return(invisible(NULL))
        }
    },
    getRows=function(query, metadata, useOffset=TRUE) {
      if (length(caches) > 1) {
          verifyCache(query)
      }
      
      tmp <- cache()$getObject(query)
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
