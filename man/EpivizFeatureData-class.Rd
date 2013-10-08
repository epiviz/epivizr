\name{EpivizFeatureData-class}
\docType{class}

\alias{class:EpivizFeatureData}
\alias{EpivizFeatureData-class}

\title{'EpivizFeatureData' objects}

\description{Encapsulate data for genomic features}

\details{The \code{object} field is constrained to be \code{\link[GenomicRanges]{SummarizedExperiment}}. Data is obtained from columns of
a determined \code{assay}. Measurement names are checked against the row names of the \code{colData} slot. The \code{rowData} is assumed
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
      \code{getMeasurements()}: Returns named character vector. Entries are of format '<name>$<column>'. Vector names are of format
      '<id>$<column>'.
    }
    
    \item{}{
      \code{parseMeasurement(msId)}: Parses measurement of format '<id>$<column>'. 
    }
    
    \item{}{
      \code{packageData(msId)}: Get data for \code{msId} for rows overlapping \code{curQuery}. Returns a list with components: \code{min}: min value for data range, \code{max}: max value for data range, \code{data}: list with slots: \code{gene}: from \code{SYMBOL} column of \code{mcols(rowData(object))}, \code{start}: feature start position, \code{end}: feature end position, \code{probe}: from \code{PROBEID} column of \code{mcols(rowData(object))}, \code{value}: numeric data value.  
    }
  }
}

\author{Hector Corrada Bravo}
