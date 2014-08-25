\name{Queue-class}
\docType{class}

\alias{class:Queue}
\alias{Queue-class}

\title{A Queue}

\description{A first-in-first-out data structure. Used by \code{\link{EpivizServer-class}} objects to queue requests.} 

\section{Fields}{
  \describe{
    \item{\code{items}}{Items stored in queue}
  }
}

\section{Methods}{
  \describe{
    \item{}{
      \code{push(item)}: Push item into queue
    }
    
    \item{}{
      \code{pop()}: Pop item on top of queue
    }

    \item{}{
	\code{peek()}: Return but not pop item on top of queue.
    }    
    \item{}{
      \code{empty()}: Empty the queue
    }
  }
}

\author{Hector Corrada Bravo}
\seealso{
  \code{\link{EpivizServer-class}}
}

\examples{
  theQ <- epivizr:::Queue$new()
  theQ$push("a")
  theQ$pop()
}
