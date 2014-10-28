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
      \code{getMeasurements()}: Returns a list describing data encapsulated by this object.}
    
    \item{}{
      \code{parseMeasurement(msId=NULL)}: Checks \code{msId==id}.
    }    
  }
}

\author{Hector Corrada Bravo}

