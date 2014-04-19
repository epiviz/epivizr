\name{EpivizDataPack-class}

\alias{class:EpivizDataPack}
\alias{EpivizDataPack-class}

\title{'EpivizDataPack' objects}

\description{Package data to send to web app}

\details{Objects of this class package the result of \code{packageData}
 for \code{\link{EpivizData-class}} for multiple measurements and prepares data for transfer to the epiviz web app. This is a virtual
 class.}

\section{Fields}{
  \describe{
    \item{\code{length}:}{Number of measurements packaged.} 
  }
}

\section{Methods}{
  \describe{
    \item{}{
      \code{set(curData, msId, index)}: Set \code{curData} as the entry on index \code{index} for measurement with \code{msId}. This is a VIRTUAL method.
    }
    
    \item{}{
      \code{getData()}: Return object for transfer to web app from packaged data. This is a VIRTUAL method.
    }
  }
}

\author{Hector Corrada Bravo}