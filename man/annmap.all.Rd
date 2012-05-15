\name{annmapAll}
\alias{annmapAll}
\alias{allArrays}
\alias{allChromosomes}
\alias{allDomains}
\alias{allEstExons}
\alias{allEstGenes}
\alias{allEstTranscripts}
\alias{allExons}
\alias{allGenes}
\alias{allPredictionTranscripts}
\alias{allProbes}
\alias{allProbesets}
\alias{allProteins}
\alias{allSymbols}
\alias{allSynonyms}
\alias{allTranscripts}
\title{
  annmap 'all' functions
}
\description{
  Get all annotations for a given feature. For example, \code{allGenes} will return data for all the genes in the genome.
}
\usage{
  allArrays( as.vector=FALSE )
  allChromosomes( as.vector=FALSE )
  allDomains( as.vector=FALSE )
  allEstExons( as.vector=FALSE )
  allEstGenes( as.vector=FALSE )
  allEstTranscripts( as.vector=FALSE )
  allExons( as.vector=FALSE )
  allGenes( as.vector=FALSE )
  allPredictionTranscripts( as.vector=FALSE )
  allProbes( as.vector=FALSE )
  allProbesets( as.vector=FALSE )
  allProteins( as.vector=FALSE )
  allSymbols( as.vector=FALSE )
  allSynonyms( as.vector=FALSE )
  allTranscripts( as.vector=FALSE )
}
\arguments{
  \item{as.vector}{ If \code{TRUE} returns a vector of database identifiers. If \code{FALSE} returns a \code{\link{GRanges}} object containing detailed annotation. }
}
\value{
  Returns a \code{vector} or \code{\link{GRanges}} object, as defined by \code{as.vector}.
}
\seealso{
  \code{\link{annmapTo}}\cr
  \code{\link{annmapDetails}}\cr
  \code{\link{annmapRange}}\cr
  \code{\link{annmapUtils}}\cr
  \code{\link{annmapFilters}}\cr
  \code{\link{GRanges}}
}
\author{
  Tim Yates
}
\examples{
  if(interactive()) {
    annmapConnect()	
    allChromosomes()
    allChromosomes(as.vector=TRUE)
  }
}
