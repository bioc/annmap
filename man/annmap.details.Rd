\name{annmapDetails}
\alias{annmapDetails}
\alias{arrayDetails}
\alias{chromosomeDetails}
\alias{domainDetails}
\alias{estExonDetails}
\alias{estGeneDetails}
\alias{estTranscriptDetails}
\alias{exonDetails}
\alias{geneDetails}
\alias{predictionTranscriptDetails}
\alias{probeDetails}
\alias{probesetDetails}
\alias{proteinDetails}
\alias{synonymDetails}
\alias{transcriptDetails}
\title{
  annmap 'details' functions
}
\description{
  Get detailed annotations for the specified features.
}
\usage{
  arrayDetails( ids, as.data.frame=FALSE )
  chromosomeDetails( ids, as.data.frame=FALSE )
  domainDetails( ids, as.data.frame=FALSE )
  estExonDetails( ids, as.data.frame=FALSE )
  estGeneDetails( ids, as.data.frame=FALSE )
  estTranscriptDetails( ids, as.data.frame=FALSE )
  exonDetails( ids, as.data.frame=FALSE )
  geneDetails( ids, as.data.frame=FALSE )
  predictionTranscriptDetails( ids, as.data.frame=FALSE )
  probeDetails( ids, as.data.frame=FALSE )
  probesetDetails( ids, as.data.frame=FALSE )
  proteinDetails( ids, as.data.frame=FALSE )
  synonymDetails( ids, as.data.frame=FALSE )
  transcriptDetails( ids, as.data.frame=FALSE )
}
\arguments{
  \item{ids}{ Database identifiers for the features of interest }
  \item{as.data.frame}{ If \code{FALSE}, data will be converted to a \code{GRanges} object if possible, otherwise a \code{data.frame} }
}
\value{
  Results in an \code{\link{GRanges}} object (or a \code{data.frame} if \code{TRUE} is passed for the second parameter), one \'row\' per feature, containing detailed annotations.
}
\seealso{
  \code{\link{annmapTo}}\cr
  \code{\link{annmapAll}}\cr
  \code{\link{annmapRange}}\cr
  \code{\link{annmapUtils}}\cr
  \code{\link{annmapFilters}}\cr
  \code{\link{GRanges}}\cr
}
\author{
  Tim Yates
}
\examples{ 
  if(interactive()) {
    annmapConnect()
    geneDetails(symbolToGene("TP53"))
  }
}
