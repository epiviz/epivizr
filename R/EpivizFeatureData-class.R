EpivizFeatureData <- setRefClass("EpivizFeatureData",
  contains="EpivizData",
  fields=list(assay="ANY",metadata="ANY"),
  methods=list(
    initialize=function(object=SummarizedExperiment(matrix(nr=0,nc=0),rowData=GRanges()),
                        assay=1, ...) {
      assay <<- assay
      
      callSuper(object=object, ...)
    },
    update=function(newObject, ...) {
      if (!is(newObject, "SummarizedExperiment"))
        stop("'newObject' must be of class 'SummarizedExperiment'")
      if(!is(rowData(newObject), "GIntervalTree"))
        rowData(newObject) <- as(rowData(newObject), "GIntervalTree")
      callSuper(newObject, ...)
    },
    .checkColumns=function(columns) {
      all(columns %in% rownames(colData(object)))
    },
    .getColumns=function() {
      rownames(colData(object))
    },
    .checkLimits=function(ylim) {
      if (!is.matrix(ylim))
        return(FALSE)
      if (nrow(ylim) != 2)
        return(FALSE)
      if (ncol(ylim) != length(columns))
        return(FALSE)
      TRUE
    },
    .getNAs=function() {
      mat <- GenomicRanges::assay(object, i=.self$assay)
      colIndex <- match(columns, rownames(colData(object)))
      namat <- is.na(mat[,colIndex])
      if (!is.matrix(namat))
        namat <- cbind(namat)
      which(rowSums(namat)>0)
    },
    .getLimits=function() {
      mat <- GenomicRanges::assay(object, i=.self$assay)
      colIndex <- match(columns, rownames(colData(object)))
      unname(sapply(colIndex, function(i) range(pretty(range(mat[,i], na.rm=TRUE)))))
    },
    plot=function(x, y, ...) {
      ms <- getMeasurements()
      if (length(ms)<2)
        stop("need at least two columns to plot")

      mgr$scatterChart(x=ms[1], y=ms[2], ...)
    }
  )
)

.valid.EpivizFeatureData.object <- function(x) {
  if(!is(x$object, "SummarizedExperiment"))
    return("'object' must be of class 'SummarizedExperiment'")
  if(!is(rowData(x$object), "GIntervalTree"))
    return("'rowData(object)' must be of class 'GIntervalTree'")
  NULL
}

.valid.EpivizFeatureData.ylim <- function(x) {
  if(!is(x$ylim, "matrix"))
    return("'ylim' must be a matrix")
  if(nrow(x$ylim) != 2)
    return("'ylim' must have two rows")
  if(ncol(x$ylim) != length(x$columns))
    return("'ylim' must have 'length(columns)' columns")
  NULL
}

.valid.EpivizFeatureData.assay <- function(x) {
  if (is.character(x$assay)) {
    if(!(x$assay %in% names(assays(x$object))))
      return("'assay' not found in 'object'")
    return(NULL)
  }

  if (x$assay > length(assays(x$object)))
    return("'assay' not found in 'object'")
  NULL
}

.valid.EpivizFeatureData <- function(x) {
  c(.valid.EpivizFeatureData.object(x),
    .valid.EpivizFeatureData.ylim(x),
    .valid.EpivizFeatureData.assay(x))
}

setValidity2("EpivizFeatureData", .valid.EpivizFeatureData)

EpivizFeatureData$methods(
    getMeasurements=function() {
      out <- lapply(columns, function(curCol) {
        m <- match(curCol, columns)
        
        list(id=curCol,
           name=curCol,
           type="feature",
           datasourceId=id,
           datasourceGroup=id,
           defaultChartType="Scatter Plot",
           annotation=NULL,
           minValue=ylim[1,m],
           maxValue=ylim[2,m],
           metadata=metadata)
    })
#     out <- paste(name, columns, sep="$")
  #    nms <- paste(id, columns, sep="__")
    #  names(out) <- nms
      out
    },
    parseMeasurement=function(msId) {
      column <- strsplit(msId, split="__")[[1]][2]
      if(!.checkColumns(column)) {
        stop("invalid parsed measurement")
      }
      column
    },
    .getMetadata=function(curHits, curMetadata) {
      if(any(!curMetadata %in% metadata))
        stop("error getting metadata")

      if (length(curHits) == 0) {
        out <- lapply(curMetadata, function (x) list())
        return(out)
      }
      out <- as.list(mcols(rowData(object))[curHits,curMetadata])
      names(out) <- curMetadata
      out
    },
  .getValues=function(curHits, measurement) {
    if (!measurement %in% columns) {
      stop("could not find measurement", measurement)
    }
    m <- match(measurement, columns)
    unname(assay(object, .self$assay)[curHits, m])
  },
    packageData=function(msId) {
      column <- parseMeasurement(msId)
      m <- match(column, columns)
      out <- list(min=unname(ylim[1,m]), max=unname(ylim[2, m]))

      if (!length(curHits)) {
        out$data <- list(gene=character(),
                         start=integer(),
                         end=integer(),
                         probe=character(),
                         value=numeric())
      } else {
        m <- match(column, rownames(colData(object)))
        out$data <- list(gene=mcols(rowData(object))$SYMBOL[curHits],
                         start=start(rowData(object))[curHits], 
                         end=end(rowData(object))[curHits],
                         probe=mcols(rowData(object))$PROBEID[curHits],
                         value=assay(object, .self$assay)[curHits,m])
      }
      out
    }
)

EpivizFeatureDataPack <- setRefClass("EpivizFeatureDataPack",
  contains="EpivizDataPack",
  fields=list(
    min="numeric",
    max="numeric",
    gene="character",
    start="integer",
    end="integer",
    probe="character",
    data="list"),
  methods=list(
    initialize=function(...) {
      callSuper(...)
      nms <- rep("", length)
      min <<- structure(rep(-6, length), names=nms)
      max <<- structure(rep(6, length), names=nms)
      gene <<- character()
      start <<- integer()
      end <<- integer()
      probe <<- character()
      data <<- structure(vector("list", length), names=nms)
    },
    set=function(curData, msId, index) {
      min[index] <<- curData$min
      names(min)[index] <<- msId

      max[index] <<- curData$max
      names(max)[index] <<- msId

      if (length(gene)==0) {
        gene <<- curData$data$gene
        start <<- curData$data$start
        end <<- curData$data$end
        probe <<- curData$data$probe
      }

      data[[index]] <<- curData$data$value
      names(data)[index] <<- msId
    },
    getData=function() {
      outData <- list(gene=gene, start=start, end=end, probe=probe)
      outData <- c(outData, data)
      list(min=min,max=max,data=outData)
    }
  )
)

EpivizFeatureData$methods(.initPack=function(length=0L) {EpivizFeatureDataPack$new(length=length)})
