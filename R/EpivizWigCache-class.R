EpivizWigCache <- setRefClass("EpivizWigCache",
  contains="EpivizDataCache",
  field=list(
    resource="BigWigFile",
    maxPoints="integer"
  ),
  methods=list(
    initialize=function(resource=BigWigFile(""), maxPoints=50000L, ...) {
      resource <<- resource
      maxPoints <<- as.integer(maxPoints)
      callSuper(...)
    },
    getObject=function(query) {
      querynm <- as.character(seqnames(query)[1])
      
      .myImport <- function(rng) {
        if (start(rng) < 1)
          start(rng) <- 1

        chrlen <- seqlengths(seqinfo(resource))[querynm]
        if (end(rng) > chrlen)
          end(rng) <- chrlen
        cacheRange <<- rng
        size <- min(width(rng), maxPoints)
        res <- suppressWarnings(summary(resource, which=rng, size=size)[[1]])
        return(res)
      }

      .chrSwitch <- function() {
        if (!querynm %in% seqnames(seqinfo(resource))) {
          cacheRange <<- GRanges()
          return(GIntervalTree(GRanges(score=numeric()))) 
        } 
        
        qwidth <- width(query)
        rng <- resize(query, width=3*qwidth, fix="center")
        return(.myImport(rng))
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
        return(.myImport(rng2))
      }
      rng2 <- resize(query, width=width(cacheRange), fix="center")
      return(.myImport(rng2))
    }
  )
)                              
    
