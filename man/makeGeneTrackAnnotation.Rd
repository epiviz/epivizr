\name{makeGeneTrackAnnotation}
\alias{makeGeneTrackAnnotation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create a gene annotation object to use as gene track on the epiviz UI.
}
\description{
Given an \code{OrganismDb} object, it creates a \code{GRanges} object in the required
format to use with the epiviz UI. This allows users to create custom genome browsers
from their Bioconductor environment.
}
\usage{
makeGeneTrackAnnotation(object, kind = c("gene", "tx"), keepSeqlevels = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
  An object of class \code{OrganismDb}.
}
  \item{kind}{return a "gene" or
    "transcript" annotation. Only gene annotations are currently
    supported.}
  \item{keepSeqlevels}{
a list of seqnames
    to retain in the gene annotation. See \code{\link[GenomeInfoDb]{keepSeqlevels}}}.
}

\details{
This function creates a \code{GRanges} object that can be used by the \code{addDevice} method
in class \code{EpivizDeviceMgr} to add a genes track to an epiviz UI.
}
\value{
An object of class \code{GRanges}, with one row per gene. It includes
columns \code{Gene} with gene symbol, and \code{Exons} including exon start and ends.
The latter is of class \code{IRangesList}.
}
\author{
Hector Corrada Bravo
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{startStandalone}
}
\examples{
\dontrun{
library(Mus.musculus)
gr <- makeGeneTrackAnnotation(Mus.musculus, keepSeqlevels=paste0("chr", c(1:19, "X", "Y")))
mgr <- startEpiviz()
mgr$addSeqinfo(seqinfo(gr))
mgr$addDevice(gr, "mm10", type="geneInfo")
}
}
