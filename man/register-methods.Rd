\name{register-methods}
\alias{register-methods}

\alias{register,GenomicRanges-method}
\alias{register,SummarizedExperiment-method}
\alias{register,ExpressionSet-method}
\alias{register,OrganismDb-method}

\title{Encapsulate data object in Epiviz}
\description{
  This method encapsulates data objects in R/bioc to be used for interactive visualization. It converts \code{\link[GenomicRanges]{GenomicRanges}} objects to \code{\link[GenomicRanges]{GIntervalTree}} objects when appropriate. It returns objects from the \code{\link{EpivizData-class}} hierarchy.
  
}
\usage{
\S4method{register}{GenomicRanges}(object, columns=NULL, type=c("block", "bp", "geneInfo"), ...)
\S4method{register}{SummarizedExperiment}(object, columns=NULL, assay=1)
\S4method{register}{ExpressionSet}(object, columns=NULL, annotation=NULL)
\S4method{register}{OrganismDb}(object, kind=c("gene", "tx"), keepSeqlevels=NULL, ...)
}
\arguments{
  \item{object}{data object to encapsulate}
  \item{columns}{columns in object to use. All columns are used if \code{NULL}}
  \item{type}{for \code{\link[GenomicRanges]{GenomicRanges}} objects, use block representation, base-pair level quantitative representation, or gene annotation (see below)}
  \item{assay}{for \code{\link[GenomicRanges]{SummarizedExperiment}} and \code{\link[Biobase]{ExpressionSet}} objects, determine which \code{assay} to use}
  \item{annotation}{for \code{\link[Biobase]{ExpressionSet}} objects, determine the annotation to use to obtain feature genomic coordinates. If \code{NULL}, then \code{annotation(object)} is used}
  \item{kind}{for \code{OrganismDb} objects, return a "gene" or
    "transcript" annotation. Only gene annotations are currently
    supported.}
  \item{keepSeqlevels}{for \code{OrganismDb} objects, a list of seqnames
    to retain in the gene annotation. See \code{\link[GenomeInfoDb]{keepSeqlevels}}}.
  \item{...}{arguments passed to \code{\link{EpivizData-class}} subclasses constructors}
}

\details{
This function converts \code{\link[GenomicRanges]{GenomicRanges}} objects to \code{\link[GenomicRanges]{GIntervalTree}} objects when appropriate. For example, the \code{rowData} slot in \code{\link[GenomicRanges]{SummarizedExperiment}} objects. Genomic coordinates for features in \code{\link[Biobase]{ExpressionSet}} objects are obtained used the \code{\link[AnnotationDbi]{AnnotationDb}} interface from the provided annotation.

Gene Annotations: Gene annotations can be provided through\code{GenomicRanges} (using \code{type="geneInfo"}) or
  \code{OrganismDb} objects. For the latter, we assume a \code{Gene}
  (\code{character}) column is available containing gene names,  and a \code{Exons} (\code{IRangesList}) column is 
  available. Annotations using \code{GRangesList} objects will be supported in the near future.
}
\value{
Returns objects from the \code{\link{EpivizData-class}} hierarchy depending on the provided object. When \code{object} is a \code{\link[GenomicRanges]{GenomicRanges}} object and \code{type=="block"}, a 
\code{\link{EpivizBlockData-class}} object is returned; a
\code{\link{EpivizBpData-class}} object is returned if \code{type=="bp"}. For \code{\link[GenomicRanges]{SummarizedExperiment}} and
\code{\link[Biobase]{ExpressionSet}} objects, a \code{\link{EpivizFeatureData-class}} object is returned. For \code{\link[GenomicRanges]{SummarizedExperiment}} objects, columns \code{SYMBOL} and \code{PROBEID} are assumed to be present in \code{mcols(rowData(object))}.
}

\author{Hector Corrada Bravo}

\seealso{
\link{EpivizData-class} and subclasses,
\link{EpivizDeviceMgr-class},
\link[GenomicRanges]{GenomicRanges},
\link[GenomicRanges]{SummarizedExperiment},
\link[GenomicRanges]{GIntervalTree},
\link[OrganismDbi]{OrganismDb}
\link[Biobase]{ExpressionSet}
}
