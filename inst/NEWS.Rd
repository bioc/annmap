\name{NEWS}
\title{NEWS file for the annmap package}
\section{Changes in version 1.0.0}{
  \itemize{
    \item{NEW}{ Initial Release to Bioconductor }
  }
}
\section{Changes in version 0.9.16}{
  \itemize{
    \item{BUG}{ ANNMAP-97 geneToExonProbeset threw an error when as.vector=TRUE }
  }
}
\section{Changes in version 0.9.15}{
  \itemize{
    \item{NEW}{ ANNMAP-94 Added \code{annmapAddConnection} method for setting up databases. }
  }
}
\section{Changes in version 0.9.14}{
  \itemize{
    \item{NEW}{ ANNMAP-90 Added \code{generateBridgeData} method for helping with BAM file imports. }
    \item{IMPROVED}{ ANNMAP-93 Refactored out some NCBI->Ensembl seqname mapping functions. }
    \item{IMPROVED}{ ANNMAP-84 Added \code{ngsBridgePlot} examples to the cookbook. }
  }
}
\section{Changes in version 0.9.13}{
  \itemize{
    \item{IMPROVED}{ ANNMAP-78 Cookbook now appears when \code{vignette( package='annmap' )} is called. }
  }
}
\section{Changes in version 0.9.12}{
  \itemize{
    \item{IMPROVED}{ ANNMAP-74, ANNMAP-75, ANNMAP-76; Fixed warnings and notes when using R-2.15 }
  }
}
\section{Changes in version 0.9.11}{
  \itemize{
    \item{IMPROVED}{ ANNMAP-73 Methods \code{generalisedNameToNCBI}, \code{generalisedNameToEnsembl}, \code{seqnameMapping}, \code{seqnamesToNCBI} and \code{seqnamesToEnsembl} added to allow easy renaming of the seqnames column of GRanges data. }
  }
}
\section{Changes in version 0.9.10}{
  \itemize{
    \item{IMPROVED}{ ANNMAP-71 Bug in geneToSymbol fixed }
  }
}
\section{Changes in version 0.9.9}{
  \itemize{
    \item{IMPROVED}{ ANNMAP-63 We now use GenomicRanges instead of IRanges }
  }
}
\section{Changes in version 0.9.8}{
  \itemize{
    \item{IMPROVED}{ ANNMAP-58 Added 4 pos translation documentation to cookbook }
    \item{BUG}{ ANNMAP-60 Webservice tests were failing }
    \item{BUG}{ ANNMAP-54 geneToExonProbeset didn't support as.vector }
  }
}
\section{Changes in version 0.9.7}{
  \itemize{
    \item{BUG}{ ANNMAP-48 Error in proteinCoordsToGenome in annmap }
    \item{BUG}{ ANNMAP-47 annmapGenePlot fails to show individual transcripts }
    \item{IMPROVED}{ ANNMAP-45 Added a NEWS.Rd file to inst }
  }
}
\section{Changes in version 0.9.6}{
  \itemize{
    \item{BUG}{ Fixed ANNMAP-44; genomicPlot crashes when called with very small overlapped areas }
  }
}
\section{Changes in version 0.9.5}{
  \itemize{
    \item{IMPROVED}{ ANNMAP-41; All InRange methods are now S3 methods which work with character, data.frame or RangedData (see ?annmap.range). }
    \item{IMPROVED}{ ANNMAP-42; genomicPlot highlights 'colour' column is now 'col' as with all other methods. }
    \item{BUG}{ ANNMAP-40; genomicPlot crashes if highlights data.frame has unexpected columns. }
  }
}
