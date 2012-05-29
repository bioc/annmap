\name{ngsPlot}
\alias{convertBamToRle}
\alias{generateBridgeData}
\alias{ngsTraceScale}
\alias{ngsTraceLabel}
\alias{ngsTracePlotter}
\alias{ngsBridgePlot}
\title{
  Plotting BAM file data alongside the features of a chromosome
}
\description{
  These functions aid plotting a-la xmapbridge but in a format that is more publication friendly
}
\usage{
  # Utility Methods
  convertBamToRle( bam.file.name, chr, start, end, chr.name.mapping=function( name ){ name } )
  generateBridgeData( xrange, bamFiles, colours=NULL, names=NULL )
  ngsTraceScale( vector.of.xbams.and.ybams )
  ngsTraceLabel( rle.data )
  ngsTracePlotter( rle.data, start, end, ylim, trace.label.properties=list(), smoothing.function=function( rle, ... ) { IRanges::runmean( rle, k=1001, endrule='constant' ) },
                     trace.clip='inherit', trace.draw.scale=FALSE, trace.bor='transparent', trace.pad=c(0,0), ... )

  # Plotting Methods
  ngsBridgePlot( xrange, data=list(), main=NULL, sub=NULL, highlights=NULL, trace.plotter=ngsTracePlotter, genome.layout.weight=4,
               trace.scale=ngsTraceScale, trace.draw.scale=NULL, trace.match.strand=TRUE, probe.plot=NULL, exon.depth.plot=genomicExonDepthPlot,
               .genes=NULL, .exons=NULL, ... )
}
\arguments{
  \item{bam.file.name}{ The name of the BAM file to read in }
  \item{chr}{ The chromosome of interest. }
  \item{start}{ The start of the region of interest  }
  \item{end}{ The end of the region of interest }
  \item{chr.name.mapping}{ The function to convert between the Annmap chr name to the chr name in the BAM file.  By default, this just uses chr supplied as the parameter, however it can be set to any function you like.  One example of this is \code{generalisedNameToNCBI} }
  \item{xrange}{ The genomic range for the x-axis. Should be a GRanges object. }
  \item{bamFiles}{ A vector containing the filenames of your BAM files. }
  \item{colours}{ A vector of colours for each file (sensible defaults will be chosen if NULL). }
  \item{names}{ A vector of names to show on the traces drawn by \code{ngsTracePlotter} }
  \item{vector.of.xbams.and.ybams}{ The \code{trace.scale} function is passes a vector of the elements of \code{xbams} and \code{ybams} concatenated together. }
  \item{rle.data}{ A list containing fields \code{rle} (the \code{\link[IRanges]{Rle}} data to be plotted), \code{name} (the name of the Trace) and \code{col} (the colour for the trace). }
  \item{ylim}{ A vector of min and max values for this plot (usually retrieved from \code{ngs.trace.scale}) }
  \item{trace.label.properties}{ Properties to be sent to the \code{\link{grid.text}} call for plotting the label on the trace.  To hide the label, this should be \code{NA}. }
  \item{smoothing.function}{ A function that generates a smoothed RLE object. }
  \item{trace.clip}{ Is the trace clipped to it's bounding box?  One of \code{'inherit'}, \code{'on'} or \code{'off'}.  See \code{\link{viewport}}. }
  \item{trace.draw.scale}{ If \code{TRUE}, \code{x} and \code{y} scales are drawn with \code{main=TRUE} (see \code{\link{grid.xaxis}} and \code{\link{grid.yaxis}}), if \code{FALSE}, then neither axis is drawn.  You can control individual axis drawing by passing a vector such as \code{trace.draw.scale='x'} to just draw the \code{x} axis.  You can also pass a list such as \code{trace.draw.scale=list(x=TRUE,y=FALSE)}, and this will draw both the \code{x} and \code{y} axis, but pass \code{main=TRUE} to the \code{grid.xaxis} call, and \code{main=FALSE} to \code{grid.yaxis} }
  \item{trace.bor}{ The colour for a box that is drawn round this trace.plot. }
  \item{trace.pad}{ A 2 element vector consisting of the number of 'lines' of padding to allow at the top and bottom of the plot respecively }
  \item{data}{ A list containing an element per trace.  Each element of this list is, in turn, passed to the \code{trace.plotter} and \code{trace.scale} functions where the plotting happens -- see \code{details}. ) }
  \item{main}{ The main title for the plot. }
  \item{sub}{ A sub-title for the plot. }
  \item{highlights}{ Highlight regions for the plot.  See \code{genomicPlot}. }
  \item{trace.plotter}{ The function to call to draw the traces (see \code{ngsTracePlotter}) }
  \item{genome.layout.weight}{ The weight for the genomic plot in the layout of this grid }
  \item{trace.scale}{ Either a function to calculate the global max for the NGS traces (see \code{ngsTraceScale}) OR a 2 element vector cntaining the min and max extent of the trace. }
  \item{trace.match.strand}{ If \code{TRUE}, we will only draw the rle data from the strand defined in xrange.  If false, we will draw all of the rle data. Can also be set to \code{'+'} or \code{'-'} to only draw the trace from the given strand (ignoring the strand of xrange). }
  \item{probe.plot}{ The function to plot the probes (see \code{\link{genomicProbePlot}}), \code{NULL} if not drawn. }
  \item{exon.depth.plot}{ The function to draw the exon depth (see \code{\link{genomicExonDepthPlot}}), \code{NULL} if not drawn. }
  \item{.genes}{ Optionally pass a list of genes to limit the plot to. }
  \item{.exons}{ An optional list of exons to limit the plot to. }
  \item{...}{ Parameters passed on to functions called by this function }
}
\details{
  \code{convertBamToRle} will take a BAM file name, and a region of interest and return a \code{list()} containing two elements, \code{'+'} and \code{'-'}. Each element will be an \code{\link[IRanges]{Rle}} object, one for each strand.

  The \code{data} parameter to \code{ngsBridgePlot} is a list of elements as defined in the \code{rle.data} parameter, one element per NGS trace, ie:

  \preformatted{
    library(grid)
    library(annmap)

    # Connect to datasource with annmapConnect()

    # Ensure we have a clean plot
    grid.newpage()

    bamFiles = c( 'data1.bam', 'data2.bam', 'data3.bam' )
    colours  = rainbow( 3, v=0.5, s=0.5 )
    data = lapply( seq_along( bamFiles ), function( idx ) {
      list( rle=convertBamToRle( bamFiles[ idx ], 'I', 40000, 100000 ),
            col=colours[ idx ],
            name=paste( 'Trace', bamFiles[ idx ] ) )
    } )
    ngsBridgePlot( RangedData( space='I', ranges=IRanges( 40000, 100000 ) ), data=data, main='Example Plot' )
  }
}
\seealso{
  \code{\link{genomicProbePlot}}, \code{\link{genomicPlot}}, \code{\link{genomicExonDepthPlot}}
}
\author{
  Tim Yates
}
