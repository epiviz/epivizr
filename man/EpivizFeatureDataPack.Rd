\name{EpivizFeatureDataPack-class}
\docType{class}

\alias{class:EpivizFeatureDataPack}
\alias{EpivizFeatureDataPack-class}

\title{'EpivizFeatureDataPack' objects}

\description{Package of feature-level data for transfer to web app.}

\section{Fields}{
  \describe{
    \item{\code{min}:}{(numeric) minimum of data range.}
    \item{\code{max}:}{(numeric) maximum of data range.}
    \item{\code{gene}:}{(character) gene symbol vector.}
    \item{\code{probe}:}{(character) probe id vector.}
    \item{\code{start}:}{(integer) feature start location.}
    \item{\code{end}:}{(integer) feature end location.}
    \item{\code{data}:}{(list) data entry for each measurement.}
  }
}

\author{Hector Corrada Bravo}

