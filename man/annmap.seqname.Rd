\name{annmapSeqname}
\alias{annmapSeqname}
\alias{generalisedNameToNCBI}
\alias{generalisedNameToEnsembl}
\alias{seqnameMapping}
\alias{seqnamesToNCBI}
\alias{seqnamesToEnsembl}
\title{
  Seqnames manipulation functions
}
\description{
  These functions allow easier manipulation of the seqnames column of a GRanges object
}
\usage{
  generalisedNameToNCBI( name, ... )
  generalisedNameToEnsembl( name, ... )
  seqnameMapping( x, mappingFunction, ... )
  seqnamesToNCBI( x )
  seqnamesToEnsembl( x )
}
\arguments{
  \item{name}{ The name to convert. }
  \item{x}{ A GRanges object to convert the seqnames of. }
  \item{mappingFunction}{ The function to do the mapping of names. }
  \item{...}{ Other arguments you may wish to send to a custom mapping function. }
}
\details{
  These functions allow simple mapping between seqnames of a GRanges object.

  The two standard derivations are \code{seqnamesToNCBI} and \code{seqnamesToEnsembl}.  The rules for mapping are:

  \preformatted{
      Ensembl      NCBI
      1       <=>  chr1
      ...
      22      <=>  chr22
      X       <=>  chrX
      Y       <=>  chrY
      MT      <=>  chrM
  }

  You can define your own mapping function and pass it as the \code{mappingFunction} parameter to
  \code{seqnameMapping} function to do your own custom mapping.

  The function \code{seqnamesToNCBI} calls \code{seqnameMapping} with \code{generalisedNameToNCBI} as the
  \code{mappingFunction}.  The function \code{seqnamesToEnsembl} uses \code{generalisedNameToEnsembl}.
}
\author{
  Tim Yates
}
\examples{
  if(interactive()) {
    annmapConnect()
    seqnamesToNCBI( symbolToGene( c( 'tp53', 'shh' ) ) )
 }
}
