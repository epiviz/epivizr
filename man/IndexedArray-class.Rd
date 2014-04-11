\name{IndexedArray-class}
\docType{class}

\alias{class:IndexedArray}
\alias{IndexedArray-class}

\title{Indexed array}

\description{This class is used by \code{\link{EpivizDeviceMgr-class}} objects to store requests callbacks.}

\section{Fields}{
  \describe{
    \item{\code{nextId}}{Next integer id to return when item is appended}
    \item{\code{items}}{Stored items}
  }
}

\section{Methods}{
  \describe{
    \item{}{
      \code{append(item)}: Append item to array, returns id of appended item
    }
    
    \item{}{
      \code{get(id)}: Return item with given id
    }
    
    \item{}{
      \code{empty()}: Empty the array
    }
  }
}

\author{Hector Corrada Bravo}
\seealso{
  \code{\link{EpivizDeviceMgr-class}}
}

\examples{
  array <- epivizr:::IndexedArray$new()
  aId <- array$append("a")
  array$get(aId)
}


