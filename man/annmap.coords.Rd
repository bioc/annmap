\name{annmapCoords}
\alias{genomeToProteinCoords}
\alias{genomeToTranscriptCoords}
\alias{proteinCoordsToGenome}
\alias{transcriptCoordsToGenome}
\title{
  annmap co-ordinate mapping functions
}
\description{
  Functions to go between Genomic, Proteomic and Transcriptual co-ordinate systems.
}
\usage{
  transcriptCoordsToGenome( transcript.ids, position=1, as.vector=FALSE, check.bounds=TRUE, truncate=TRUE, cds=FALSE )
  genomeToTranscriptCoords( position, transcript.ids, as.vector=FALSE, check.bounds=TRUE, end=c( 'none', 'both', '5', '3' ) )
  proteinCoordsToGenome( protein.ids, position=1, as.vector=FALSE, check.bounds=TRUE, truncate=TRUE )
  genomeToProteinCoords( position, protein.ids, as.vector=FALSE, check.bounds=TRUE )
}
\arguments{
  \item{transcript.ids}{ A vector of transcript.ids (or a RangedData object of transcripts returned from another annmap function)}
  \item{position}{ The position of interest (either a genomic position for both of the \code{genomeToXXXX} methods,
  or a protein or transcript sequence position for the other two methods )}
  \item{as.vector}{ Should the returned data be in the form of a vector (if \code{TRUE}) or a RangedData object (if \code{FALSE}) }
  \item{check.bounds}{ If \code{TRUE}, any postion outside the range of the protein/transcript will cause a warning to be issued and \code{NA} returned. }
  \item{end}{ Should the UTR be taken in to account when calculating the location, one of (\code{"none"}, \code{"both"}, \code{"3"} or \code{"5"}). Defaults to \code{none}. }
  \item{truncate}{ If \code{truncate=TRUE}, any lengths beyond the end of the transcript or protein will be set to the last residue}
  \item{cds}{ If \code{cds=TRUE} then only the coding exons (or sub-regions of exons that are coding) are taken in to account. }
  \item{protein.ids}{ A vector of protein.ids (or a RangedData object of proteins returned from another annmap function) }
}
\details{
  The mapping functions need to deal with mappings that fall outside a
  transcript or protein (or within an intron). When \code{as.vector=FALSE} these are
  identified as \code{NA} in the results. Since \code{RangedData} objects cannot
  represent \code{NA} or missing values, when \code{as.vector=FALSE}, all locations
  which cannot be mapped are dropped from the result.
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
    # Get the gene for 'tp53'
    gene        = symbolToGene( 'tp53' )
    # And the transcripts for this gene
    transcripts = geneToTranscript( symbolToGene( 'tp53' ) )
    # And the proteins for this transcript
    proteins    = transcriptToProtein( transcripts )

    # get the transcript coords for the transcripts of this gene, at the start of this gene
    genomeToTranscriptCoords( start( gene ), transcripts, as.vector=TRUE )
    #Returns a vector:
    # ENST00000413465 ENST00000359597 ENST00000504290 ENST00000510385 ENST00000504937
    #            1018              NA              NA              NA              NA     
    # ENST00000269305 ENST00000455263 ENST00000420246 ENST00000445888 ENST00000396473
    #              NA              NA              NA              NA              NA     
    # ENST00000545858 ENST00000419024 ENST00000509690 ENST00000514944 ENST00000505014
    #              NA              NA              NA              NA              NA
    # ENST00000414315 ENST00000508793 ENST00000503591 
    #              NA              NA              NA

    # With as.vector=FALSE
    genomeToTranscriptCoords( start( gene ), transcripts )
    # RangedData with 1 row and 1 value column across 1 space
    #             space       ranges | coord.space
    #       <character>    <IRanges> | <character>
    # 1 ENST00000413465 [1018, 1018] |  transcript

    genomeToProteinCoords( start( gene ), proteins, as.vector=TRUE )
    # ENSP00000410739 ENSP00000352610 ENSP00000269305 ENSP00000398846 ENSP00000391127
    #             340              NA              NA              NA              NA
    # ENSP00000391478 ENSP00000379735 ENSP00000437792 ENSP00000402130 ENSP00000425104
    #              NA              NA              NA              NA              NA
    # ENSP00000423862 ENSP00000394195 ENSP00000424104 ENSP00000426252 
    #              NA              NA              NA              NA 

    # With as.vector=FALSE
    genomeToProteinCoords( start( gene ), proteins )
    # RangedData with 1 row and 2 value columns across 1 space
    #             space     ranges |     frame coord.space
    #       <character>  <IRanges> | <numeric> <character>
    # 1 ENSP00000410739 [340, 340] |         0     protein
  }
}
