\name{EpivizGeneInfoData-class}
\docType{class}

\alias{class:EpivizGeneInfoData-class}
\alias{EpivizGeneInfoData-class}

\title{'EpivizGeneInfoData' objects}

\description{Encapsulate gene annotation}

\details{This class extends \code{\link{EpivizTrackData-class}}. Like
  its super-class, the data containing object is constrained to be of
  class \code{\link[GenomicRanges]{GIntervalTree}}. Ranges contained
  in the object correspond to the genomic interval spanning all
  annotated exons for the gene. Columns \code{Gene} and \code{Exons} are
  assumed to be part of the \code{elementMetadata} of the data
  containing object. Column \code{Gene} (\code{character}) is used as gene names in the
  UI, column \code{Exons} (\code{IRangesList}) is used to annotate exon
  start and ends.}

\section{Methods}{
  \describe{
    \item{}{
      \code{plot()}: Calls the \code{genesChart} method of \code{\link{EpivizDeviceMgr-class}}.
    }

    \item{}{
      \code{getMeasurements()}: Returns a list describing data
      encapsulated by this object.
    }
  }
}

\author{Hector Corrada bravo}

  
  