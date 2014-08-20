\name{EpivizChart-class}

\alias{class:EpivizChart}
\alias{EpivizChart-class}

\title{'EpivizChart' objects}

\description{Encapsulate information about charts added to web app.}

\section{Fields}{
  \describe{
    \item{\code{measurements}:}{List describing measurements displayed in chart.}
    \item{\code{id}:}{(character) id assigned by session manager to chart. Use \code{getId} method to get id.}
    \item{\code{mgr}:}{(\code{\link{EpivizDeviceMgr}}) session manager object.}
    \item{\code{inDevice}:}{(logical) \code{TRUE} if chart is defined within an \code{\link{EpivizDevice-class}} object.}
    \item{\code{type}:}{(character) chart type.}
  }
}

\section{Methods}{
  \describe{
    \item{}{\code{setId(id)}:}{set \code{id}. Internal use only.}
    \item{}{\code{getId()}:}{get \code{id}.}
    \item{}{\code{setInDevice(x)}:}{set \code{inDevice} field. Internal use only.}
  }
}

\author{Hector Corrada Bravo}

