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
      \code{getMeasurements()}: Returns named character vector. Entries are of format '<name>$<column>'. Vector names are of format
      '<id>$<column>'.
    }
    
    \item{}{
      \code{parseMeasurement(msId)}: Parses measurement of format '<id>$<column>'. 
    }
    
    \item{}{
      \code{packageData(msId)}: Get data for \code{msId} for rows overlapping \code{curQuery}. Returns a list with components: \code{min}: min value for data range, \code{max}: max value for data range, \code{data}: list with slots: \code{bp}: genomic location,
      \code{value}: numeric data value.
    }
  }
}

\author{Hector Corrada Bravo}
