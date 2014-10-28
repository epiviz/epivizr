\name{startStandalone}
\alias{startStandalone}

\title{
Start the standalone epiviz interface
}
\description{
Create an epiviz session manager for the epiviz web application served
from the localhost.
}
\usage{
startStandalone(geneInfo = NULL, geneInfoName = "", seqinfo = NULL, chr = "", start = 1, end = 1, start.args = list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{geneInfo}{
An object containing gene annotation information. See \code{\link{register-methods}} for information on objects permitted.
}
  \item{geneInfoName}{
(character) The name of the gene annotation to display on the UI.
}
  \item{seqinfo}{
\code{\link[GenomeInfoDb]{Seqinfo-class}} object containing sequence names and lengths.
Ignored if \code{geneInfo} is not NULL.
}
  \item{chr}{
Sequence name to load the UI.
}
  \item{start}{
Starting genomic position when loading UI.
}
  \item{end}{
Ending genomic position when loading UI.
}
  \item{start.args}{
List containing other arguments to pass \code{\link{startEpiviz}}.
}
  \item{\dots}{
Arguments passed to \code{addDevice} method of \code{\link{EpivizDeviceMgr}} when
adding gene annotation. For instance, \code{keepSeqlevels}.
}
}
\value{
An object of class \linkS4class{EpivizDeviceMgr}.	
}
\author{
Hector Corrada Bravo
}

\seealso{
\link{EpivizDeviceMgr-class}
\link{register-methods}
}
\examples{
\dontrun{
library(Mus.musculus)
mgr <- startStandalone(Mus.musculus, geneInfoName="mm10",
    keepSeqlevels=paste0("chr",c(1:19,"X","Y")))	
}
}
