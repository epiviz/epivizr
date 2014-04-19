\name{EpivizServer-class}
\docType{class}

\alias{class:EpivizServer}
\alias{EpivizServer-class}

\title{EpivizServer objects}

\description{
This class encapsulates the websocket connection between the R/bioc
session and the epiviz web app.
}

\details{
Epiviz uses websockets to connect R/bioc sessions to the epiviz web app. Objects of this class encapsulates websocket objects defined in the \code{\link[httpuv]{httpuv}} package. The Epiviz session manager defined in class \code{\link{EpivizDeviceMgr}} calls methods in this class to send requests to the web app or to respond to requests. Requests are created by this class, so actions defined in the JS side are paralleled closely here (this makes the \linkS4class{EpivizDeviceMgr}) independent of the specifics in the JS side.
}


\section{Fields}{
  \describe{
    \item{\code{port}:}{Port for websocket connection (default 7312)}
    \item{\code{websocket}:}{A \code{\link[httpuv]{WebSocket}} object from the \code{\link[httpuv]{httpuv}} package}
    \item{\code{server}:}{Internal use only: A server object from the \code{httpuv} package, set by the \code{\link[httpuv]{startServer}} method}
    \item{\code{interrupted}:}{Internal use only: has an interrupt been issued}
    \item{\code{socketConnected}:}{Internal use only: is the websocket connected}
    \item{\code{msgCallback}:}{Internal use only: callback function on websocket message receive, set by \code{bindMgr} method}
    \item{\code{requestQueue}:}{Internal use only: Queue for unsent requests}
    \item{\code{tryPorts}:}{Interval use only: Try more ports when opening websocket server if port requested by user is in use.}
  }
}

\section{Methods}{
  \describe{
    \item{}{
      \code{startServer(...)}: Sets up the server and websocket objects from \code{\link[httpuv]{httpuv}} package. Specifically, it sets the \code{server} field by calling \code{\link[httpuv]{startServer}} passing \code{port} and
      a set of callbacks, including a \code{onWSOpen} callback that sets the \code{websocket} field and sets
      the \code{msgCallback} method as the \code{websocket$onMessage} callback.
    }
    \item{}{
      \code{stopServer()}: Stops the websocket server, calling \code{\link[httpuv]{stopServer}}.
    }
    \item{}{
      \code{service()}: A blocking loop for the websocket to listen and serve requests. Calls \code{\link[httpuv]{service}}.
    }
    \item{}{
      \code{stopService()}: Signals an interrupt so the \code{service} loop is stopped.
    }
    \item{}{
      \code{runServer()}: A convenience function that runs \code{startServer} and \code{service}. \code{stopServer} is called on exit.
    }
  \item{}{
    \code{isClosed()}: Returns \code{TRUE} if server connection is closed
  }
  \item{}{
    \code{bindManager(mgr)}: Sets the \code{msgCallback} on websocket message callback. It calls methods from argument \code{mgr}, usually an object of class \code{\link{EpivizDeviceMgr}}. At minimum it must be a \code{list} with components \code{verbose} and \code{getData}. 
  }
  \item{}{
    \code{sendRequest(request)}: Converts argument \code{request} to JSON and sends through \code{websocket} if connected, otherwise, adds to \code{requestQueue}.
  }
  \item{}{
    \code{sendRequestsInQueue()}: Sends all requests currently in the request queue.
  }
  \item{}{
    \code{emptyRequestQueue()}: Empties request queue.
  }
}
}

\section{Web App Requests}{
  In all below, \code{requestId} is a callback function id set by the session manager.
  \describe{
    \item{}{
      \code{addMeasurements(requestId, msType, measurements)}: adds a set of measurements to the web app. \code{msType} indicates the type of measurement, see the \code{typeMap} field of \code{\link{EpivizDeviceMgr}} objects for valid measurement types. \code{measurements} is a named \code{character} vector of measurement names. \code{names(measuremnts)} are the measurement ids.
      }
    
    \item{}{
      \code{rmMeasurements(requestId, measurements, msType)}: removes a set of measurements. Arguments are as \code{addMeasurements} above.
    }
    
    \item{}{
      \code{addChart(requestId, chartType, measurements)}: adds a chart to the web app. \code{chartType} indicates the type of chart to add, see \code{\link{EpivizChart-class}} for valid chart types. \code{measurements} is a \code{character} vector containing measurement ids displayed in the chart.
    }
    
    \item{}{
      \code{rmChart(requestId, chartId)}: removes chart from the web app.\code{chartId} specifies the id of the chart to remove. This is returned in response to an \code{addChart} request.
    }
    
    \item{}{
      \code{clearChartCaches(requestId, chartIds)}: clears data caches for given charts. Used when objects in R/bioc session are updated.\code{chartIds} is a vector of chart ids.
    }
    
    \item{}{
      \code{navigate(requestId, chr, start, end)}: Make web app navigate to given genomic region.
    }
  }
}

\author{Hector Corrada Bravo}
\seealso{
  \link{EpivizDeviceMgr-class},
  \link{EpivizChart-class},
  \link[httpuv]{httpuv},
  \link[httpuv]{startServer},
  \link[httpuv]{service},
  \link[httpuv]{WebSocket}
}

\examples{
  \dontrun{
  mgr <- list(getData=function(measurements, chr, start, end) {
                        return(chr)
                      },
              verbose=TRUE)

  server <- epivizr:::EpivizServer$new(port=7123L)
  server$bindManager(mgr)
  server$startServer()
  
  browseURL("http://localhost:7123/")
  tryCatch(server$service(), interrupt=function(int) invisible())
  }
}