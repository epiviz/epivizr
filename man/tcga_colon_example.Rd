\name{tcga_colon_example}
\alias{tcga_colon_example}
\alias{colon_blocks}
\alias{colon_curves}
\docType{data}
\title{Example methylation data for epivizr vignette}
\description{
This package contains example results from methylation analysis of human chromosome 11 using the \code{\link[minfi]{minfi-package}} package of TCGA 450k beadarray samples.
}
\usage{data(tcga_colon_example)}
\format{
  Two \code{\link[GenomicRanges]{GRanges}} objects. The first \code{colon_blocks}, described large regions of methylation difference between tumor and normal samples. It has the following as metadata:
  \describe{
    \item{\code{value}}{average smooth methylation difference within block}
    \item{\code{area}}{block area estimate (abs(value) * length)}
    \item{\code{cluster}}{id of cluster blockgroup within which block occurs}
    \item{\code{indexStart}}{index of first cluster in block}
    \item{\code{indexEnd}}{index of last cluster in block}
    \item{\code{L}}{number of clusters in block}
    \item{\code{clusterL}}{number of probes in block}
    \item{\code{p.value}}{permutation p.value based on difference conditioned on length}
    \item{\code{fwer}}{family-wise error rate estimate based on difference conditioned on length}
    \item{\code{p.valueArea}}{permutation p.value based on area}
    \item{\code{fwerArea}}{family-wise error rate estimate based on area}
  }
  
  The second, \code{colon_curves}, is probe cluster-level methylation estimates. It has the following as element metadata:
  \describe{
    \item{\code{id}}{probe cluster id}
    \item{\code{type}}{probe cluster type}
    \item{\code{blockgroup}}{probe cluster block group}
    \item{\code{diff}}{raw methylation percentage difference between normal and tumor}
    \item{\code{smooth}}{smooth methylation percentage difference between normal and tumor}
    \item{\code{normalMean}}{mean methylation estimate for normal samples}
    \item{\code{cancerMean}}{mean methylation estimate for cancer samples}
  }
}
\source{
TCGA project: \url{https://tcga-data.nci.nih.gov/tcga/}
}
\seealso{\link[minfi]{minfi-package}}
\author{Hector Corrada Bravo}
\examples{
data(tcga_colon_example)
show(colon_blocks)
show(colon_curves)
}
\keyword{datasets}
