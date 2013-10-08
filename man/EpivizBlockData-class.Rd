\name{EpivizBlockData-class}
\docType{class}

\alias{class:EpivizBlockData}
\alias{EpivizBlockData-class}

\title{'EpivizBlockData' objects}

\description{Data displayed only as genomic regions (no quantitative data)}

\details{This class extends \code{\link{EpivizTrackData-class}}.
Like its super-class, the data containing object is constrained to be
of class \code{\link[GenomicRanges]{GIntervalTree}}. Its
\code{columns} field is constrained to be \code{NULL}.}

\section{Methods}{
  \describe{
    \item{}{
      \code{plot()}: Calls the \code{blockChart} method of 
      \code{\link{EpivizDeviceMgr-class}}.
    }
    
    \item{}{
      \code{getMeasurements()}: The measurements names and ids for data encapsulated by this object. Returns a named character vector of length 1 containing \code{name} with \code{id} as the vector name.
    }
    
    \item{}{
      \code{parseMeasurement(msId=NULL)}: Checks \code{msId==id}.
    }
    
    \item{}{
      \code{packageData(msId=NULL)}: Returns list with slots \code{start} and \code{end} for ranges overlapping \code{curQuery}. 
    }
  }
}

\author{Hector Corrada Bravo}

