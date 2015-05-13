\name{tcga_colon_expression}
\alias{tcga_colon_expression}
\alias{colonSE}
\docType{data}
\title{
Example exon-level RNAseq data from TCGA project for epivizr.
}
\description{
A \code{\link[SummarizedExperiment]{RangedSummarizedExperiment}} object containing exon-level
counts from RNAseq data from the TCGA project. Only exons in human chromosome 11 are included.
}
\usage{data(tcga_colon_expression)}
\format{
  A \code{\link[SummarizedExperiment]{RangedSummarizedExperiment}} object with 12,800 rows (exons) and 40 samples.
  \describe{
    \item{assay(colonSE)}{exon-level count matrix}
    \item{colData(colonSE)}{a \code{DataFrame} containing sample information. Normal/Tumor status is given in column \code{sample_type}}
  }
}
\source{
TCGA project: \url{https://tcga-data.nci.nih.gov/tcga/}
}
\examples{
data(tcga_colon_expression)
show(colonSE)
table(colData(colonSE)$sample_type)
}
\keyword{datasets}
