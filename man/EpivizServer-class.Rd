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
Epiviz uses websockets to connect R/bioc sessions to the epiviz web app. Objects of this class encapsulates websocket objects defined in the \code{\link[httpuv]{httpuv}} package. The Epiviz session manager defined in class \code{\link{EpivizDeviceMgr}} calls methods in this class to send requests to the web app or to respond to requests. Can also be executed in \code{standalone} mode, serving the epiviz web app JS through the \code{httpuv} server.
}


\section{Fields}{
  \describe{
    \item{\code{port}:}{Port for websocket connection (default 7312)}
    \item{\code{websocket}:}{A \code{\link[httpuv]{WebSocket}} object from the \code{\link[httpuv]{httpuv}} package}
    \item{\code{server}:}{Internal use only: A server object from the \code{httpuv} package, set by the \code{\link[httpuv]{startServer}} method}
    \item{\code{interrupted}:}{Internal use only: has an interrupt been issued}
    \item{\code{socketConnected}:}{Internal use only: is the websocket connected}
    \item{\code{msgCallback}:}{Internal use only: callback functions on websocket or http requests. Created using \code{shiny:::HandlerManager}}
    \item{\code{requestQueue}:}{Internal use only: Queue for unsent requests}
    \item{\code{tryPorts}:}{Internal use only: Try more ports when opening websocket server if port requested by user is in use.}
    \item{\code{daemonized}:}{Internal use only: is it running in daemonized mode}
    \item{\code{standalone}:}{Internal use only: is it running in standalone mode}
  }
}

\section{Methods}{
  \describe{
    \item{}{
      \code{startServer(...)}: Sets up the server and websocket objects from \code{\link[httpuv]{httpuv}} package. Specifically, it sets the \code{server} field by calling \code{\link[httpuv]{startServer}} passing \code{port} and
      a set of callbacks, including a \code{onWSOpen} callback that sets the \code{websocket} field and sets
      the \code{msgCallback} method as the \code{websocket$onMessage} callback. In standalone mode, it also creates a callback for http requests.
    }
    \item{}{
      \code{stopServer()}: Stops the websocket server, calling \code{\link[httpuv]{stopServer}}.
    }
    \item{}{
      \code{service()}: A blocking (if not running daemonized) loop for the websocket to listen and serve requests. Calls \code{\link[httpuv]{service}}.
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
    \code{bindManager(mgr)}: Sets the \code{msgCallback} on websocket message callback. It calls methods from argument \code{mgr}, usually an object of class \code{\link{EpivizDeviceMgr}}. Methods currently called from\code{mgr} are: \code{getMeasurements,getRows,getValues,getSeqInfos} and \code{getAllData}. The latter is used only in testing. 
  }
  \item{}{
    \code{sendRequest(request)}: Converts argument \code{request} to JSON and pushes it to the \code{requestQueue}.
  }
  \item{}{
    \code{popRequest()}: Pop a request from \code{requestQueue} and send to web app.
  }
  \item{}{
    \code{emptyRequestQueue()}: Empties request queue.
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