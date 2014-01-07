\name{annmapUtr}
\alias{transcriptToUtrRange}
\alias{transcriptToUtrExon}
\alias{transcriptToCodingRange}
\alias{transcriptToCodingExon}
\alias{utrProbesets}
\alias{codingProbesets}
\alias{nonIntronicTranscriptLength}
\alias{nonIntronicGeneLength}
\title{
  annmap coding functions
}
\description{
  Functions to deal with coding regions and UTRs
}
\usage{
  transcriptToUtrRange( ids, end=c( "both", "5", "3" ), as.data.frame=FALSE, on.translation.error=stop )
  transcriptToUtrExon( ids, end=c( 'both', '5', '3' ), as.vector=FALSE, on.translation.error=stop )
  transcriptToCodingRange( ids, end=c( "both", "5", "3" ), as.data.frame=FALSE, on.translation.error=stop )
  transcriptToCodingExon( ids, end=c( 'both', '5', '3' ), as.vector=FALSE, on.translation.error=stop )
  utrProbesets( probesets, transcripts, end=c( "both", "5", "3" ), on.translation.error=stop )
  codingProbesets( probesets, transcripts, end=c( "both", "5", "3" ), on.translation.error=stop )
  nonIntronicTranscriptLength( ids, end=c( 'none', 'both', '5', '3' ), on.translation.error=stop )
  nonIntronicGeneLength( ids )
}
\arguments{
  \item{ids}{ A vector of Transcript Names, or a RangedData object of Transcripts returned from another annmap call. }
  \item{as.data.frame}{ If \code{FALSE}, data will be converted to a \code{RangedData} object if possible, otherwise a \code{data.frame} }
  \item{as.vector}{ If \code{TRUE} returns a vector of database identifiers. If \code{FALSE} returns a \code{link{GRanges}} object containing detailed annotation. }
  \item{probesets}{ An optional vector of Probeset Names, or a RangedData object of Probesets returned from another annmap call. }
  \item{transcripts}{ An optional vector of Transcript Names, or a RangedData object of Transcripts returned from another annmap call. }
  \item{end}{ Which end (\code{"both"}, \code{"3"} or \code{"5"}) of the Transcript(s) you are interested in (defaults to \code{both}). }
  \item{on.translation.error}{ A function to call with a character vector explaining the problem if one is encountered with the translation locations in the database. }
}
\details{
  The first two functions given here, \code{transcriptToUtrRange} and \code{transcriptToCodingRange} return the transcripts
  of interest, with their ranges adjusted depending on the UTR of each.

  With \code{transcriptToUtrRange}, a RangedData object is returned with the name of the transcript, the end in question, and
  the genomic location of that UTR.  If \code{both} is passed as the \code{end} parameter, then each transcript will generate up to
  two rows in the returned object.  It may return less than two rows if the \code{end} parameter is used, or if there is no UTR for the
  end specified. (A Transcript with no UTR will return zero results)

  The \code{transcriptToCodingRange} function returns the same as calling \code{transcriptDetails}, but with the start and end locations
  modified by the range of the UTR.  If \code{end} is passed, then only the UTR at this end will be taken into consideration and used to modify
  the returned location.

  The \code{transcriptToCodingExon} and \code{transcriptToUtrExon} functions return the exons for each transcript limited to only those exons (or partions thereof) which are coding or part of the UTR.

  \code{utrProbesets} and \code{codingProbesets} are functions to find or filter probesets which have probes targeting the type of region
  specified by the function name.

  A call to \code{utrProbesets} with a list of Probesets will return those probesets that have at least one probe hitting the UTR of any transcript.

  A call to \code{utrProbesets} with a list of Probesets and a list of Transcripts will return those probesets the have at least one probe
  hitting the UTR of any of the specified Transcripts.

  A call to \code{utrProbesets} with only the \code{probesets} parameter omitted, will return all probesets which have at least one probe in the UTR
  region of the specified Transcripts.

  You cannot omit both the Probesets and Transcripts parameters simultaneously.

  The \code{codingProbesets} method does the inverse of the \code{utrProbesets} function: it returns probesets having at least one probe
  in the coding region of a Transcipt (or the specified Transcripts).

  Note that the UTR of a Transcript includes the intronic UTR regions, and the coding region of a Transcript includes the
  intronic coding regions.

  This means that \code{utrProbesets} and \code{codingProbesets} can sometimes return \code{intronic} and/or \code{intergenic} probesets.
  These can be removed with a call to the appropriate filter function (see examples).
  
  All \code{unreliable} probesets are automatically removed by these functions before mapping.

  Calling \code{nonIntronicTranscriptLength} will return the length of the exons (coding can be specified via the \code{end} parameter) in a given list of transcripts.

  And \code{nonIntronicGeneLength} will give the length of all exons in a given gene when overlaps are taken into account (so two exactly overlapping exons will count once for the length)
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
}
\examples{
  if(interactive()) {
    # Only return exonic probesets hitting the UTRs of ENST00000414566
    exonic( utrProbesets( NULL, "ENST00000414566" ) )
  }
}
