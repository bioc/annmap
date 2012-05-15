\name{annmapRange}
\alias{annmapRange}

\alias{domainInRange}
\alias{domainInRange,RangedData-method}
\alias{domainInRange,GRanges-method}
\alias{domainInRange,character-method}
\alias{domainInRange,data.frame-method}
\alias{domainInRange,factor-method}

\alias{estExonInRange}
\alias{estExonInRange,RangedData-method}
\alias{estExonInRange,GRanges-method}
\alias{estExonInRange,character-method}
\alias{estExonInRange,data.frame-method}
\alias{estExonInRange,factor-method}

\alias{estGeneInRange}
\alias{estGeneInRange,RangedData-method}
\alias{estGeneInRange,GRanges-method}
\alias{estGeneInRange,character-method}
\alias{estGeneInRange,data.frame-method}
\alias{estGeneInRange,factor-method}

\alias{estTranscriptInRange}
\alias{estTranscriptInRange,RangedData-method}
\alias{estTranscriptInRange,GRanges-method}
\alias{estTranscriptInRange,character-method}
\alias{estTranscriptInRange,data.frame-method}
\alias{estTranscriptInRange,factor-method}

\alias{exonInRange}
\alias{exonInRange,RangedData-method}
\alias{exonInRange,GRanges-method}
\alias{exonInRange,character-method}
\alias{exonInRange,data.frame-method}
\alias{exonInRange,factor-method}

\alias{geneInRange}
\alias{geneInRange,RangedData-method}
\alias{geneInRange,GRanges-method}
\alias{geneInRange,character-method}
\alias{geneInRange,data.frame-method}
\alias{geneInRange,factor-method}

\alias{predictionTranscriptInRange}
\alias{predictionTranscriptInRange,RangedData-method}
\alias{predictionTranscriptInRange,GRanges-method}
\alias{predictionTranscriptInRange,character-method}
\alias{predictionTranscriptInRange,data.frame-method}
\alias{predictionTranscriptInRange,factor-method}

\alias{probesetInRange}
\alias{probesetInRange,RangedData-method}
\alias{probesetInRange,GRanges-method}
\alias{probesetInRange,character-method}
\alias{probesetInRange,data.frame-method}
\alias{probesetInRange,factor-method}

\alias{probeInRange}
\alias{probeInRange,RangedData-method}
\alias{probeInRange,GRanges-method}
\alias{probeInRange,character-method}
\alias{probeInRange,data.frame-method}
\alias{probeInRange,factor-method}

\alias{proteinInRange}
\alias{proteinInRange,RangedData-method}
\alias{proteinInRange,GRanges-method}
\alias{proteinInRange,character-method}
\alias{proteinInRange,data.frame-method}
\alias{proteinInRange,factor-method}

\alias{transcriptInRange}
\alias{transcriptInRange,RangedData-method}
\alias{transcriptInRange,GRanges-method}
\alias{transcriptInRange,character-method}
\alias{transcriptInRange,data.frame-method}
\alias{transcriptInRange,factor-method}

\title{
  annmap 'range' functions
}
\description{
  Get the features within the specified genome coordinates.
}
\usage{
  domainInRange( x, ..., as.vector = FALSE )
  \S4method{domainInRange}{GRanges}( x, as.vector=FALSE )
  \S4method{domainInRange}{RangedData}( x, as.vector=FALSE )
  \S4method{domainInRange}{character}( x, start, end, strand, ..., as.vector=FALSE )
  \S4method{domainInRange}{data.frame}( x, as.vector=FALSE )
  \S4method{domainInRange}{factor}( x, start, end, strand, ..., as.vector=FALSE )

  estExonInRange( x, ..., as.vector = FALSE )
  \S4method{estExonInRange}{GRanges}( x, as.vector=FALSE )
  \S4method{estExonInRange}{RangedData}( x, as.vector=FALSE )
  \S4method{estExonInRange}{character}( x, start, end, strand, ..., as.vector=FALSE )
  \S4method{estExonInRange}{data.frame}( x, as.vector=FALSE )
  \S4method{estExonInRange}{factor}( x, start, end, strand, ..., as.vector=FALSE )

  estGeneInRange( x, ..., as.vector = FALSE )
  \S4method{estGeneInRange}{GRanges}( x, as.vector=FALSE )
  \S4method{estGeneInRange}{RangedData}( x, as.vector=FALSE )
  \S4method{estGeneInRange}{character}( x, start, end, strand, ..., as.vector=FALSE )
  \S4method{estGeneInRange}{data.frame}( x, as.vector=FALSE )
  \S4method{estGeneInRange}{factor}( x, start, end, strand, ..., as.vector=FALSE )

  estTranscriptInRange( x, ..., as.vector = FALSE )
  \S4method{estTranscriptInRange}{GRanges}( x, as.vector=FALSE )
  \S4method{estTranscriptInRange}{RangedData}( x, as.vector=FALSE )
  \S4method{estTranscriptInRange}{character}( x, start, end, strand, ..., as.vector=FALSE )
  \S4method{estTranscriptInRange}{data.frame}( x, as.vector=FALSE )
  \S4method{estTranscriptInRange}{factor}( x, start, end, strand, ..., as.vector=FALSE )

  exonInRange( x, ..., as.vector = FALSE )
  \S4method{exonInRange}{GRanges}( x, as.vector=FALSE )
  \S4method{exonInRange}{RangedData}( x, as.vector=FALSE )
  \S4method{exonInRange}{character}( x, start, end, strand, ..., as.vector=FALSE )
  \S4method{exonInRange}{data.frame}( x, as.vector=FALSE )
  \S4method{exonInRange}{factor}( x, start, end, strand, ..., as.vector=FALSE )

  geneInRange( x, ..., as.vector = FALSE )
  \S4method{geneInRange}{GRanges}( x, as.vector=FALSE )
  \S4method{geneInRange}{RangedData}( x, as.vector=FALSE )
  \S4method{geneInRange}{character}( x, start, end, strand, ..., as.vector=FALSE )
  \S4method{geneInRange}{data.frame}( x, as.vector=FALSE )
  \S4method{geneInRange}{factor}( x, start, end, strand, ..., as.vector=FALSE )

  predictionTranscriptInRange( x, ..., as.vector = FALSE )
  \S4method{predictionTranscriptInRange}{GRanges}( x, as.vector=FALSE )
  \S4method{predictionTranscriptInRange}{RangedData}( x, as.vector=FALSE )
  \S4method{predictionTranscriptInRange}{character}( x, start, end, strand, ..., as.vector=FALSE )
  \S4method{predictionTranscriptInRange}{data.frame}( x, as.vector=FALSE )
  \S4method{predictionTranscriptInRange}{factor}( x, start, end, strand, ..., as.vector=FALSE )

  probesetInRange( x, ..., as.vector = FALSE )
  \S4method{probesetInRange}{GRanges}( x, as.vector=FALSE )
  \S4method{probesetInRange}{RangedData}( x, as.vector=FALSE )
  \S4method{probesetInRange}{character}( x, start, end, strand, ..., as.vector=FALSE )
  \S4method{probesetInRange}{data.frame}( x, as.vector=FALSE )
  \S4method{probesetInRange}{factor}( x, start, end, strand, ..., as.vector=FALSE )

  probeInRange( x, ..., as.vector = FALSE )
  \S4method{probeInRange}{GRanges}( x, as.vector=FALSE )
  \S4method{probeInRange}{RangedData}( x, as.vector=FALSE )
  \S4method{probeInRange}{character}( x, start, end, strand, ..., as.vector=FALSE )
  \S4method{probeInRange}{data.frame}( x, as.vector=FALSE )
  \S4method{probeInRange}{factor}( x, start, end, strand, ..., as.vector=FALSE )

  proteinInRange( x, ..., as.vector = FALSE )
  \S4method{proteinInRange}{GRanges}( x, as.vector=FALSE )
  \S4method{proteinInRange}{RangedData}( x, as.vector=FALSE )
  \S4method{proteinInRange}{character}( x, start, end, strand, ..., as.vector=FALSE )
  \S4method{proteinInRange}{data.frame}( x, as.vector=FALSE )
  \S4method{proteinInRange}{factor}( x, start, end, strand, ..., as.vector=FALSE )

  transcriptInRange( x, ..., as.vector = FALSE )
  \S4method{transcriptInRange}{GRanges}( x, as.vector=FALSE )
  \S4method{transcriptInRange}{RangedData}( x, as.vector=FALSE )
  \S4method{transcriptInRange}{character}( x, start, end, strand, ..., as.vector=FALSE )
  \S4method{transcriptInRange}{data.frame}( x, as.vector=FALSE )
  \S4method{transcriptInRange}{factor}( x, start, end, strand, ..., as.vector=FALSE )
}
\arguments{
  \item{as.vector}{ If \code{TRUE} returns a vector of database identifiers. If \code{FALSE} returns a \code{\link{GRanges}} object containing detailed annotation. }
  \item{x}{ The name of the chromosome of interest -- in the case of the \code{factor} or \code{character} variants), or a \code{GRanges} object  or \code{data.frame} containing location information. In the case of a \code{data.frame}, columns must be named \code{chr} or \code{chromosome_name}, followed by \code{start}, \code{end} and \code{strand}.  RangedData objects must contain a strand in their meta-data. And strand must be \code{1} or \code{-1} in all cases arart from \code{GRanges} where it obviously has to be \code{+} or \code{-}. }
  \item{start}{ Start of the region }
  \item{end}{ End of the region }
  \item{strand}{ 1 == top stand, -1 == bottom strand }
  \item{...}{ The ellipsis is to allow this multi-method style of programming. }
}
\details{
  Find all the specified features within a given region of the genome. For all functions except \code{probeInRange}, features that fall on the boundaries of the region (i.e. are partially overlapping) are returned too. For \code{probeInRange} probes that span the start of the range are NOT returned (but those spanning the end of the range are).

  The function \code{annmapRangeApply} makes it possible to map any of these functions down the rows of a \code{\link{RangedData}} or \code{\link{GRanges}} object. The defaults are set up so that it will handle the output of one of the \code{InRange} methods here. This makes it easy to nest functions, for example, to find all genes in a given region of the the genome, and then find the exon array probes that map to those genes (see below). 
}
\value{
 Returns a \code{\link{GRanges}} object, one \'row\' per feature, containing detailed annotations, or a \code{vector} of identifiers, depending on the value of \code{as.vector}.
}
\seealso{
  \code{\link{annmapTo}}\cr
  \code{\link{annmapDetails}}\cr
  \code{\link{annmapAll}}\cr
  \code{\link{annmapUtils}}\cr
  \code{\link{annmapFilters}}\cr
  \code{\link{RangedData}}
  \code{\link{GRanges}}
}
\author{
  Tim Yates
}
\examples{
  if(interactive()) {
    annmapConnect()

    r = geneInRange( '17', 7510000, 7550000, 1 )

    # Can take equal length vectors as parameters
    geneInRange( c( '17', 'X' ), c( 7510000, 1000000 ), c( 7550000, 1500000 ), c( -1, -1 ) )

    # Or a data.frame
    df = data.frame( chr=c( '17', 'X' ), start=c( 7510000, 1000000 ), end=c( 7550000, 1500000 ), strand=c( -1, -1 ) )
    geneInRange( df )

    # Or RangedData objects
    transcriptInRange( geneDetails( symbolToGene( c( 'tp53', 'ssh' ) ) ) )
  }
}
