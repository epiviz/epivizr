#' Example methylation data (blocks) for epivizr vignette.
#' 
#' Example results from methylation analysis of human chromosome 11 using the [`minfi::minfi-package`] package 
#' of TCGA 450k beadarray samples. This object contains large regions of methylation difference between tumor 
#' and normal samples obtained from [`minfi::blockFinder()`].
#' 
#' @format A [`GRanges`] object with 129 and \code{mcols}:
#' 
#' \describe{
#'  \item{\code{value}}{average smooth methylation difference within block}
#'  \item{\code{area}}{block area estimate (abs(value) * length)}
#'  \item{\code{cluster}}{id of cluster blockgroup within which block occurs}
#'  \item{\code{indexStart}}{index of first cluster in block}
#'  \item{\code{indexEnd}}{index of last cluster in block}
#'  \item{\code{L}}{number of clusters in block}
#'  \item{\code{clusterL}}{number of probes in block}
#'  \item{\code{p.value}}{permutation p.value based on difference conditioned on length}
#'  \item{\code{fwer}}{family-wise error rate estimate based on difference conditioned on length}
#'  \item{\code{p.valueArea}}{permutation p.value based on area}
#'  \item{\code{fwerArea}}{family-wise error rate estimate based on area}
#' }
#' @source TCGA project: \url{https://tcga-data.nci.nih.gov/tcga/}
#' @name tcga_colon_blocks
#' @docType data
#' @usage data(tcga_colon_blocks)
#' @md
NULL

#' Example methylation data (smoothed methylation levels) for epivizr vignette
#' 
#' Example results from methylation analysis of human chromosome 11 using the [`minfi::minfi-package`] package 
#' of TCGA 450k beadarray samples. This object contains probe cluster level methylation estimates from 
#' [`minfi::blockFinder()`].
#' 
#' @format A [`GRanges`] object with 7135 ranges and \code{mcols}:
#'
#' \describe{
#'  \item{\code{id}}{probe cluster id}
#'  \item{\code{type}}{probe cluster type}
#'  \item{\code{blockgroup}}{probe cluster block group}
#'  \item{\code{diff}}{raw methylation percentage difference between normal and tumor}
#'  \item{\code{smooth}}{smooth methylation percentage difference between normal and tumor}
#'  \item{\code{normalMean}}{mean methylation estimate for normal samples}
#'  \item{\code{cancerMean}}{mean methylation estimate for cancer samples} 
#' }
#' 
#' @source TCGA project: \url{https://tcga-data.nci.nih.gov/tcga/}
#' @docType data
#' @name tcga_colon_curves
#' @usage data(tcga_colon_curves)
#' @md
NULL

#' Example exon-level RNAseq data from TCGA project for epivizr vignette.
#' 
#' A [`SummarizedExperiment::RangedSummarizedExperiment`] object containing exon-level
#' counts from RNAseq data for colon tumor and normal tissue from the TCGA project. 
#' Only exons in human chromosome 11 are included.
#' 
#' @format A [`SummarizedExperiment::RangedSummarizedExperiment`] object with 12,800 rows (exons) and 40 samples.
#'  \describe{
#'    \item{\code{assay(tcga_colon_expression)}}{exon-level count matrix}
#'    \item{\code{colData(tcga_colon_expression)}}{a \code{DataFrame} containing sample information. 
#'      Normal/Tumor status is given in column \code{sample_type}}
#'  }
#'  
#' @source TCGA project: \url{https://tcga-data.nci.nih.gov/tcga/}
#' @return A [`SummarizedExperiment::RangedSummarizedExperiment`] object.
#' @docType data
#' @name tcga_colon_expression
#' @usage data(tcga_colon_expression)
#' @md
NULL
