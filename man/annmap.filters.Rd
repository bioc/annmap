\name{annmapFilters}
\alias{annmapFilters}
\alias{exonic}
\alias{hasProbes}
\alias{hasProbesAtleast}
\alias{hasProbesIn}
\alias{hasProbesBetween}
\alias{intergenic}
\alias{intronic}
\alias{isExonic}
\alias{isIntergenic}
\alias{isIntronic}
\alias{isUnreliable}
\alias{unreliable}

\title{
  annmap 'filter' functions
}
\description{
  Functions to filter exon array probeset names by the genome features they correspond to.
}
\usage{
  exonic( probesets, exclude=FALSE )
  hasProbes( probesets, num.probes=4, exclude=FALSE )
  hasProbesAtleast( probesets, num.probes=4, exclude=FALSE )
  hasProbesIn( probesets, num.probes=c( 1, 2, 3, 4 ), exclude=FALSE )
  hasProbesBetween( probesets, min.probes=1, max.probes=4, exclude=FALSE, inclusive=TRUE )
  intergenic( probesets, exclude=FALSE )
  intronic( probesets, exclude=FALSE )
  isExonic( probesets )
  isIntergenic( probesets )
  isIntronic( probesets )
  isUnreliable( probesets )
  unreliable( probesets, exclude=FALSE )
}
\arguments{
  \item{probesets}{ A \code{vector} of probesets to filter }
  \item{num.probes}{ The required number of probes to have in the probeset }
  \item{exclude}{ If \code{FALSE}, then return a list containing only those probesets matching the filter. If \code{TRUE} then return only those that don\'t match the filter  }
  \item{min.probes}{ Minimum number of probes within a probeset}
  \item{max.probes}{ Maximum number of probes within a probeset}
  \item{inclusive}{ Whether to include the extremes of the range in the search or not}
}
\details{
 Probesets are classified according to whether they map to known genes. The function \code{exonic} filters for probesets for which all probes match once (and only once) to the genome, and every probe hits an exon. Note that this means that a probeset that hits more than one exon, will be flagged as exonic. All probes in \code{intronic} probesets hit the genome once (and once only), and all probes hit a gene - however one or more probes hit an intron. \code{intergenic} probesets hit the genome once (and once only) but one or more probes miss a gene compeletely. \code{unreliable} probesets comprise those that have at least one probe that does not align to the genome, or one or more probes that align at multiple loci (multiply targeted). 

The functions \code{is.exonic}, \code{is.intronic} and \code{is.intergenic}, return a logical vector classifying the supplied probesets. 

The functions \code{has.probes}, \code{has.probes.in} and \code{has.probes.between} can be used to filter a set of probesets according to the numbers of probes they contain.
}
\seealso{
  \code{\link{annmapTo}}\cr
  \code{\link{annmapDetails}}\cr
  \code{\link{annmapAll}}\cr
  \code{\link{annmapRange}}\cr
  \code{\link{annmapFilters}}\cr
}
\author{
  Tim Yates
  Crispin J. Miller
}
\examples{
  if(interactive()){
    annmapConnect()
    ps <- geneToProbeset(symbolToGene("TP53"))
    exonic(ps)
    intronic(ps)
    intergenic(ps)
    unreliable(ps)
    isExonic(ps)
    isIntronic(ps)
    isIntergenic(ps)
    isUnreliable(ps)
    hasProbes(ps)
    hasProbesIn(ps,1:3)
    hasProbesBetween(ps,2,3)
    hasProbesAtleast(ps,4)
  }
}



