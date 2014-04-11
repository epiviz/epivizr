\name{EpivizDevice-class}
\docType{class}

\alias{class:EpivizDevice}
\alias{EpivizDevice-class}

\title{'EpivizDevice' objects}

\description{Encapsulate both measurement and chart for display in epiviz web app}

\details{This provides a convenience interface to the measurement/chart interface. All measurements for a given object are encapsulated in an \code{\link{EpivizData-class}} object, and its \code{plot}
method is called to create an \code{\link{EpivizChart-class}} object.}

\section{Fields}{
  \describe{
    \item{\code{msObject}:}{(\code{\link{EpivizData-class}}) object encapsulating measuremnts.}
    \item{\code{chartObject}:}{(\code{\link{EpivizChart-class}}) object encapsulating a chart.}
    \item{\code{id}:}{(character) device id set by session manager. Use \code{getId} method to get id.}
  }
}

\section{Methods}{
  \describe{
    \item{}{
      \code{getMsObject()}: Return \code{msObject}.
    }
    
    \item{}{
      \code{getChartObject()}: Return \code{chartObject}.
    }
    
    \item{}{
      \code{getMsId()}: Return id of \code{msObject}.
    }
    
    \item{}{
      \code{getChartId()}: Return id of \code{chartObject}.
    }
    
    \item{}{
      \code{setId(id)}: Set device id. For internal use only.
    }
    
    \item{}{
      \code{getId()}: Return device object id.
    }
    
    \item{}{
      \code{update(newObject, sendRequest=TRUE)}: Update data object in \code{msObject}. Calls the \code{update} method of \code{msObject}.
    }
  }
}

\author{Hector Corrada Bravo}
