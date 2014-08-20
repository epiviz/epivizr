\name{EpivizBpData-class}
\docType{class}

\alias{class:EpivizBpData}
\alias{EpivizBpData-class}

\title{'EpivizBpData' objects}

\description{Base-pair resolution quantitative data.}

\details{This class extends \code{\link{EpivizTrackData-class}} directly. As such, its \code{object} field is constrained to contain a \code{\link[GenomicRanges]{GIntervalTree}} object. Quantitative data for each genomic position is assumed to be stored in the \code{elementMetadata} slot of \code{object}.}

\section{Methods}{
  \describe{
    \item{}{
      \code{.checkColumns(columns)}: Checks argument \code{columns} is a subset of \code{names(mcols(object)).}
    }
    
    \item{}{
      \code{.getColumns()}: Returns \code{names(mcols(object))}.
    }
    
    \item{}{
      \code{plot()}: Calls the \code{lineChart} method of \code{\link{EpivizDeviceMgr-class}} with \code{columns} as argument.
    }
    
    \item{}{
      \code{getMeasurements()}: Returns a list describing data encapsulated by this object.
    }
  }    
}

\author{Hector Corrada Bravo}
