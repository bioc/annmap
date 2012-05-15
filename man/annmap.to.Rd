\name{annmapTo}
\alias{annmapTo}
\alias{arrayToProbeset}
\alias{domainToGene}
\alias{domainToProbeset}
\alias{domainToProtein}
\alias{domainToTranscript}
\alias{estExonToEstGene}
\alias{estExonToEstTranscript}
\alias{estExonToProbeset}
\alias{estGeneToEstExon}
\alias{estGeneToEstTranscript}
\alias{estGeneToProbeset}
\alias{estTranscriptToEstExon}
\alias{estTranscriptToEstGene}
\alias{estTranscriptToProbeset}
\alias{exonToGene}
\alias{exonToProbeset}
\alias{exonToTranscript}
\alias{geneToDomain}
\alias{geneToExon}
\alias{geneToExonProbeset}
\alias{geneToExonProbesetExpr}
\alias{geneToProbeset}
\alias{geneToProtein}
\alias{geneToSymbol}
\alias{geneToSynonym}
\alias{geneToTranscript}
\alias{predictionTranscriptToPredictionExon}
\alias{predictionTranscriptToProbeset}
\alias{probeToHit}
\alias{probeToProbeset}
\alias{probesetToCdnatranscript}
\alias{probesetToDomain}
\alias{probesetToEstExon}
\alias{probesetToEstGene}
\alias{probesetToEstTranscript}
\alias{probesetToExon}
\alias{probesetToGene}
\alias{probesetToHit}
\alias{probesetToPredictionTranscript}
\alias{probesetToProbe}
\alias{probesetToProtein}
\alias{probesetToTranscript}
\alias{proteinToDomain}
\alias{proteinToGene}
\alias{proteinToProbeset}
\alias{proteinToTranscript}
\alias{symbolToEstGene}
\alias{symbolToEstTranscript}
\alias{symbolToGene}
\alias{symbolToTranscript}
\alias{synonymToEstGene}
\alias{synonymToEstTranscript}
\alias{synonymToGene}
\alias{synonymToTranscript}
\alias{transcriptToCdnaprobeset}
\alias{transcriptToDomain}
\alias{transcriptToExon}
\alias{transcriptToExonProbeset}
\alias{transcriptToGene}
\alias{transcriptToProbeset}
\alias{transcriptToProtein}
\alias{transcriptToSynonym}
\alias{transcriptToTranslatedprobes}
\title{
  annmap 'to' functions
}
\description{
  Map between the different levels of annotation in Annmap. For example, given a vector of gene identifiers, \code{geneToExon} 
  will return the exons in those genes.
}
\usage{
  arrayToProbeset( ids, as.vector=FALSE )
  domainToGene( ids, as.vector=FALSE )
  domainToProbeset( ids, as.vector=FALSE )
  domainToProtein( ids, as.vector=FALSE )
  domainToTranscript( ids, as.vector=FALSE )
  estExonToEstGene( ids, as.vector=FALSE )
  estExonToEstTranscript( ids, as.vector=FALSE )
  estExonToProbeset( ids, as.vector=FALSE )
  estGeneToEstExon( ids, as.vector=FALSE )
  estGeneToEstTranscript( ids, as.vector=FALSE )
  estGeneToProbeset( ids, as.vector=FALSE )
  estTranscriptToEstExon( ids, as.vector=FALSE )
  estTranscriptToEstGene( ids, as.vector=FALSE )
  estTranscriptToProbeset( ids, as.vector=FALSE )
  exonToGene( ids, as.vector=FALSE )
  exonToProbeset( ids, as.vector=FALSE )
  exonToTranscript( ids, as.vector=FALSE )
  geneToDomain( ids, as.vector=FALSE )
  geneToExon( ids, as.vector=FALSE )
  geneToExonProbeset( ids, as.vector=FALSE, probes.min=4 )
  geneToExonProbesetExpr( x, ids, probes.min=4 )
  geneToProbeset( ids, as.vector=FALSE )
  geneToProtein( ids, as.vector=FALSE )
  geneToSymbol( ids )
  geneToSynonym( ids, as.vector=FALSE )
  geneToTranscript( ids, as.vector=FALSE )
  predictionTranscriptToPredictionExon( ids )
  predictionTranscriptToProbeset( ids, as.vector=FALSE )
  probeToHit( ids, as.data.frame=FALSE )
  probeToProbeset( ids, as.vector=FALSE )
  probesetToCdnatranscript( ids, as.vector=FALSE, rm.unreliable=TRUE )
  probesetToDomain( ids, as.vector=FALSE, rm.unreliable=TRUE )
  probesetToEstExon( ids, as.vector=FALSE, rm.unreliable=TRUE )
  probesetToEstGene( ids, as.vector=FALSE, rm.unreliable=TRUE )
  probesetToEstTranscript( ids, as.vector=FALSE, rm.unreliable=TRUE )
  probesetToExon( ids, as.vector=FALSE, rm.unreliable=TRUE )
  probesetToGene( ids, as.vector=FALSE, rm.unreliable=TRUE )
  probesetToHit( ids, as.data.frame=FALSE, rm.unreliable=TRUE )
  probesetToPredictionTranscript( ids, as.vector=FALSE, rm.unreliable=TRUE )
  probesetToProbe( ids, as.vector=FALSE )
  probesetToProtein( ids, as.vector=FALSE, rm.unreliable=TRUE )
  probesetToTranscript( ids, as.vector=FALSE, rm.unreliable=TRUE )
  proteinToDomain( ids, as.vector=FALSE )
  proteinToGene( ids, as.vector=FALSE )
  proteinToProbeset( ids, as.vector=FALSE )
  proteinToTranscript( ids, as.vector=FALSE )
  symbolToEstGene( ids, as.vector=FALSE )
  symbolToEstTranscript( ids, as.vector=FALSE )
  symbolToGene( ids, as.vector=FALSE )
  symbolToTranscript( ids, as.vector=FALSE )
  synonymToEstGene( ids, as.vector=FALSE )
  synonymToEstTranscript( ids, as.vector=FALSE )
  synonymToGene( ids, as.vector=FALSE )
  synonymToTranscript( ids, as.vector=FALSE )
  transcriptToCdnaprobeset( ids, as.vector=FALSE )
  transcriptToDomain( ids, as.vector=FALSE )
  transcriptToExon( ids, as.vector=FALSE )
  transcriptToExonProbeset( ids, as.vector=FALSE, probes.min=4 )
  transcriptToGene( ids, as.vector=FALSE )
  transcriptToProbeset( ids, as.vector=FALSE )
  transcriptToProtein( ids, as.vector=FALSE )
  transcriptToSynonym( ids, as.vector=FALSE )
  transcriptToTranslatedprobes( ids )
}
\arguments{
  \item{as.vector}{ If \code{TRUE} returns a vector of database identifiers. If \code{FALSE}
                    returns a \code{link{RangedData}} object containing detailed annotation. }
  \item{as.data.frame}{ Where a vector is inappropriate for the data type, the option to return the data as a plain \code{data.frame} in place of a \code{GRanges} object is given. }
  \item{ids}{ Database identifiers to map from. Can be either a vector of database
              identifiers, or a \code{\link{GRanges}} object.}
  \item{probes.min}{ How many probes need to match before the probeset is returned. }
  \item{rm.unreliable}{ If \code{TRUE}, the input probeset list is filtered, and all
                        unreliable probesets are removed. }
  \item{x}{ An \code{\link[Biobase:class.ExpressionSet]{ExpressionSet}} object or a
            \code{matrix} containing expression data. If the latter, then the rownames
            must specify the exon array probeset names. } 
}
\details{
  In most cases, these functions should be self-explantory. However, by default, the mappings involving probes and probesets 
  do some filtering of the data. This means that probesets which have one or more probes that don't match to the genome, or which 
  match to multiple loci, are removed (see \code{\link{unreliable}} for more details).

  The function \code{transcriptToTranslatedprobes} returns a list of \code{GRanges} objects (one for each transcript) containing each probe that hits that translated transcripts and the relative start and end locations.
}
\value{
  Results in an \code{\link{GRanges}} object, one row per feature, containing detailed annotations, or a \code{vector}, as defined by \code{as.vector}.

}
\seealso{
  \code{\link{annmapDetails}}\cr
  \code{\link{annmapAll}}\cr
  \code{\link{annmapRange}}\cr
  \code{\link{annmapUtils}}\cr
  \code{\link{annmapFilters}}\cr
  \code{link{GRanges}}
}
\author{
  Tim Yates
}
\examples{
  if(interactive()) {
    annmapConnect()
    geneToExon(symbolToGene("TP53"))
  } 
}
