\name{genomicPlotting}
\alias{genomicPlot}
\alias{genomicExonDepthPlot}
\alias{genomicProbePlot}
\title{
  Plotting a section of a chromosome.
}
\description{
  These functions are used when we need to plot one or both strands of a section of chromosome.
}
\usage{
  genomicPlot( xrange, gene.area.height=NULL, gene.layout.padding=100, highlights=NULL, draw.opposite.strand=FALSE, exon.depth.plot=genomicExonDepthPlot,
                padding.lines=1, .genes=NULL, .exons=NULL, invert.strands=FALSE, draw.scale=TRUE, ... )
  genomicExonDepthPlot( .exons, start, end, exon.depth.alpha=0.1, exon.depth.col='black', ... )
  genomicProbePlot( probes, start, end, probe.col='green', probe.alpha=0.3, ... )
}
\arguments{
  \item{xrange}{ An IRanges object representing the region of interest (with a strand if reqd) }
  \item{gene.area.height}{ If \code{NULL} then both strands to max height of either of them, else if \code{NA} then both strands limited to their implied height otherwise, if an \code{integer} then both strands limited to the specified height }
  \item{gene.layout.padding}{ How much space (in bases) needs to be between each gene in a layer.  Needed to stop gene names overlapping }
  \item{highlights}{ You can pass this a \code{data.frame} of values to render as dummy genes in the view. Columns MUST include \code{start}, \code{end}, \code{strand} and \code{name}. It may also optionally include the columns \code{col} to specify a per-gene background colour, or \code{bor} to specify the colour to be used for the gene border and the label text. If these two are not passed, sensible defaults are chosen automatically. }
  \item{draw.opposite.strand}{ Do we draw a washed out representation of the other strand. Only applies if \code{strand( xrange ) != '*'} }
  \item{exon.depth.plot}{ Should we draw the exondepth? set to \code{NULL} if not }
  \item{padding.lines}{ How much padding above and below the plot (in grid lines) }
  \item{.genes}{ Optionally pass in the pre-loaded genes and exons (then we skip loading them in this function) }
  \item{.exons}{ The exons that are to be used }
  \item{invert.strands}{ Should the forward strand be on the bottom of the plot? }
  \item{draw.scale}{ Draw a scale between the two strands? }
  \item{...}{ Parameters passed on to functions called by this function }
  \item{exon.depth.alpha}{ The transparency for the exon.depth rectangles }
  \item{exon.depth.col}{ The color for the exon.depth rectangles }
  \item{start}{ The start of the region of interest  }
  \item{end}{ The end of the region of interest }
  \item{probe.alpha}{ How transparent should probes be rendered? }
  \item{probe.col}{ The colour to use for probes. }
  \item{probes}{ The probes for the region of interest (as a \code{data.frame}). }
}
\author{
  Tim Yates
}
