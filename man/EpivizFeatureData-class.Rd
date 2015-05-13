\name{EpivizFeatureData-class}
\docType{class}

\alias{class:EpivizFeatureData}
\alias{EpivizFeatureData-class}

\title{'EpivizFeatureData' objects}

\description{Encapsulate data for genomic features}

\details{The \code{object} field is constrained to be \code{\link[SummarizedExperiment]{RangedSummarizedExperiment}}. Data is obtained from columns of
a determined \code{assay}. Measurement names are checked against the row names of the \code{colData} slot. The \code{rowRanges} is assumed
to be a \code{\link[GenomicRanges]{GIntervalTree}} object, its \code{mcols} must contain columns \code{PROBEID} and \code{SYMBOL}.}

\section{Fields}{
  \describe{
    \item{\code{assay}:}{(integer or character) indicating assay from which data is obtained}
  }
}

\section{Methods}{
  \describe{
    \item{}{
      \code{.checkColumns(columns)}: Checks \code{columns} are a subset of \code{names(colData(object))}.
    }
    
    \item{}{
      \code{.getColumns()}: Returns \code{names(colData(object))}.
    }
    
    \item{}{
      \code{plot()}: Calls the \code{scatterPlot} method of \code{\link{EpivizDeviceMgr-class}} with the first measurements in \code{columns} as \code{x} and \code{y}.
    }
    
    \item{}{
      \code{getMeasurements()}: Returns list describing data encapsulated in this object.
    }
  }
}

\author{Hector Corrada Bravo}
