EpivizWigCache <- setRefClass("EpivizWigCache",
  contains="EpivizDataCache",
  field=list(
    resource="BigWigFile",
      windowSize="integer"
  ),
  methods=list(
    initialize=function(resource=BigWigFile(""), windowSize=0L, ...) {
      resource <<- resource
      windowSize <<- as.integer(windowSize)
      callSuper(...)
    },
    readResource=function(rng, action="replace") {
        if (start(rng) < 1)
            start(rng) <- 1

        querynm <- as.character(seqnames(rng)[1])
        chrlen <- seqlengths(seqinfo(resource))[querynm]
        if (end(rng) > chrlen)
            end(rng) <- chrlen

        if (action=="replace") {
            cacheRange <<- rng
        } else if (action=="right") {
            end(cacheRange) <<- end(rng)
        } else {
            start(cacheRange) <<- start(rng)
        }
        
        if (windowSize > 0) {
            size <- ceiling(width(rng) / windowSize)
            res <- suppressWarnings(summary(resource, which=rng, size=size)[[1]])
        } else {
            res <- suppressWarnings(import.bw(resource, which=rng, as="GRanges"))
        }

        if (action != "replace") {
            naIndex <- is.na(res$score)
            if (any(naIndex)) {
                res <- res[!naIndex,]
            }
        }
        
        return(list(res, action))
    },
    getObject=function(query) {
      querynm <- as.character(seqnames(query)[1])
      
      .chrSwitch <- function() {
        if (!querynm %in% seqnames(seqinfo(resource))) {
          cacheRange <<- GRanges()
          return(GIntervalTree(GRanges(score=numeric()))) 
        } 
        
        qwidth <- width(query)
        rng <- resize(query, width=3*qwidth, fix="center")
        return(readResource(rng, action="replace"))
      }

      if (length(seqnames(cacheRange)) == 0) {
        cachenm <- ""
      } else {
        cachenm <- as.character(seqnames(cacheRange)[1])
      }
      
      if (querynm != cachenm) {
        return(.chrSwitch())
      }

      rng <- setdiff(query, cacheRange)
      if (length(rng) == 0) {
        # query contained in cache
        return(NULL)
      }

      if (length(rng) > 1) {
        # xxx|cache|xxx
        rng2 <- resize(query, width=3*width(query), fix="center")
        return(readResource(rng2, action="replace"))
      }

      # anything else
      if (end(rng) > end(cacheRange)) {
          ## cache|xxx
          rng2 <- resize(rng, fix="start", width=2*width(rng))
          return(readResource(rng2, action="right"))
      }
      rng2 <- resize(rng, fix="end", width=2*width(rng))
      return(readResource(rng2, action="left"))
    }
  )
)                              
    
