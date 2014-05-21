EpivizWigData <- setRefClass("EpivizWigData",
  contains="EpivizBpData",
  fields=list(file="BigWigFile", cacheRange="GRanges"),                             
  methods=list(
    initialize=function(object=GIntervalTree(GRanges(score=numeric())), file=BigWigFile(), ...) {
      file <<- file
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
    import=function(query) {
      querynm <- as.character(seqnames(query)[1])
      if (length(seqnames(cacheRange)) == 0) {
        cachenm <- ""
      } else {
        cachenm <- as.character(seqnames(cacheRange)[1])
      }
      
      if (querynm != cachenm) {
        if (!querynm %in% seqnames(seqinfo(file))) {
          newObj <- GIntervalTree(GRanges(score=numeric()))
          rng <- GRanges()
        } else {
          rng <- GRanges(seqnames=querynm,
                         IRanges(start=1, width=seqlengths(seqinfo(file))[querynm]))
          epivizrMsg("importing data for seq ", querynm, " from bigwig file")
          newObj <- import.bw(file, selection=as(rng,"BigWigSelection"))
        }
        update(newObj)
        cacheRange <<- rng
      }
    },
    getRows=function(query, metadata) {
      import(query)
      callSuper(query, metadata)
    },
    .getMetadata=function(curHits, curMetadata) {
      return(NULL)
    },
    plot=function(...) {
      ms <- getMeasurements()
      mgr$lineChart(ms, ...)
    }
 )
)                             
