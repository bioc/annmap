\name{NEWS}
\title{NEWS file for the annmap package}
\section{Changes in version 1.36.99}{
  \itemize{
    \item{IMPROVED}{ Version number changed to reflect Bioconductor repository }
  }
}
\section{Changes in version 1.5.11}{
  \itemize{
    \item{IMPROVED}{ ANNMAP-121 The data is now cached in a dedicated directory instead of in the home directory}
    \item{IMPROVED}{ ANNMAP-121 URLs for the project and the download location for the database was updated}
  }
}
\section{Changes in version 1.5.10}{
  \itemize{
    \item{IMPROVED}{ ANNMAP-121 Allow setting of WS base url for local testing }
  }
}
\section{Changes in version 1.5.8}{
  \itemize{
    \item{IMPROVED}{ ANNMAP-119 as.vector=TRUE results are now named }
    \item{IMPROVED}{ ANNMAP-120 genomeToTranscriptCoords could handle coding regions }
  }
}
\section{Changes in version 1.5.7}{
  \itemize{
    \item{BUG}{ Check for uniqueness in transcriptToCodingExon, transcriptToCodingRange, transcriptToUtrExon and transcriptToUtrRange }
  }
}
\section{Changes in version 1.5.6}{
  \itemize{
    \item{NEW}{ Added \code{transcriptToUtrExon} }
    \item{IMPROVED}{ Sequence removed from \code{transcriptToCodingExon} }
  }
}
\section{Changes in version 1.5.5}{
  \itemize{
    \item{IMPROVED}{ ANNMAP-115 Added method \code{geneToGeneRegionTrack} to generate a list of \code{Gviz} \code{GeneRegionTrack} elements that can be passed to \code{plotTracks} }
  }
}
\section{Changes in version 1.5.4}{
  \itemize{
    \item{BUG}{ ANNMAP-118 transcriptToCodingRange( ..., end='3' ) returns whole transcript for untranslated transcripts. }    \item{IMPROVED}{ ANNMAP-114 Added additional logging when using webservice }
    \item{IMPROVED}{ ANNMAP-116 Introduce function(s) to calculate the coding length of a gene(s) -- See: \code{?nonIntronicGeneLength} and \code{?nonIntronicTranscriptLength}. }
  }
}
\section{Changes in version 1.5.3}{
  \itemize{
    \item{IMPROVED}{ ANNMAP-114 Added additional logging when using webservice }
    \item{IMPROVED}{ ANNMAP-113 Added documentation to cookbook for webservice }
    \item{IMPROVED}{ webservice json to data.frame speed improvements }
  }
}
\section{Changes in version 1.2.1}{
  \itemize{
    \item{BUG}{ ANNMAP-112 transcriptCoordsToGenome and proteinCoordsToGenome both failed when returning a GRanges object as I had strand as a numeric, rather than an integer. }
  }
}
\section{Changes in version 1.1.1}{
  \itemize{
    \item{IMPROVED}{ genomicPlot when passed a .genes data.frame used to call geneDetails on it to ensure the data was in the correct format.  This has been removed, so that you can pass custom regions in and have them plotted on the graph. }
  }
}
\section{Changes in version 0.99.3}{
  \itemize{
    \item{BUG}{ ANNMAP-110 XXXInRange( NULL ) used to throw an error.  It now returns NULL. }
    \item{BUG}{ ANNMAP-111 Fix crash when translation data is incorrect and causes us to fall off the translation exon when finding the UTR/Coding range. }
  }
}
\section{Changes in version 0.99.2}{
  \itemize{
    \item{BUG}{ ANNMAP-109 geneToSymbol when passed a vector of gene IDs, returned the symbols in the order of the genes start location rather than the order the function was called with. }
  }
}
\section{Changes in version 0.99.1}{
  \itemize{
    \item{BUG}{ ANNMAP-101 ngsBridgePlot called with no genes in range had a huge border. }
    \item{BUG}{ ANNMAP-106 Empty ngsBridgePlot trace should draw line at 0. }
    \item{BUG}{ ANNMAP-107 transcriptToCodingRange was very slow. }
  }
}
\section{Changes in version 0.99.0}{
  \itemize{
    \item{NEW}{ Initial Release to Bioconductor }
    \item{BUG}{ ANNMAP-98 A call to transcriptToTranslatedprobes for transcripts in HPRT1 caused a crash }
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
