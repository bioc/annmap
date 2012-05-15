\name{spliceIndex}
\alias{spliceIndex}
\alias{spliceGroupIndex}
\title{
  Splice indexing
}
\description{
  Calculates the splicing index for the probesets in one or more genes, as defined in the Affymetrix white paper "Alternative Transcript Analysis Methods for Exon Arrays".
}
\usage{
  spliceGroupIndex( x, group.column, members )
  spliceIndex( x, ids, group, gps, group.index.fn=spliceGroupIndex, median.gene=FALSE, median.probeset=FALSE, unlogged=TRUE )
}
\arguments{
  \item{x}{ eSet containing expression data }
  \item{group.column}{ a column name for the group data }
  \item{members}{ a set of arrays }
  \item{ids}{ Character vector of Ensembl gene names }
  \item{group}{ If defined, the column name in the ExpressionSet's pData object in which to look for gps }
  \item{gps}{ The two sets of arrays to compare }
  \item{group.index.fn}{ a method which, when passed an ExpressionSet (from the Biobase package), a column name for the group data and a set of arrays, will return the indices of interest }
  \item{median.gene}{ Use the median instead of the mean when calculating averages across genes }
  \item{median.probeset}{ Use the median instead of the mean when calculating averages across probesets in each replicate group }
  \item{unlogged}{ Unlog the expression data before calculating the splicing index (and then re-log afterwards) }
}
\details{
    The splicing index gives a measure of the difference in expression level for each probeset
    in a gene between two sets of arrays, relative to the gene-level average in each set. This
    is calculated only for those probesets that are defined as exonic (See \code{\link{exonic}}).

    The two sets of arrays can be specified in two ways: First, by using numeric indices defining
    the appropriate columns in the expression data. This is done by supplying these as a list to
    gps (e.g. gps=list(1:3,4:6) will calculate the splicing index between arrays 1,2,3 and 4,5,6.
    Alternatively, the annotation in the phenoData object from x can be used
    (e.g. group="treatment",gps=c("a","b") will compare between the arrays labelled
    'a', and 'b' in the 'treatment' column of pData(x)).

    The implementation also calculates a p.value and t.statistic for each probeset; these are
    returned alongside the splicing index.

    By default, the splicing index is calculated using the mean across genes and samples. Specifying
    median.gene=TRUE or median.probeset=TRUE will use the median instead (for the gene or probeset
    level averages, respectively). It is calculated using the unlogged data, unless unlogged=FALSE.
    This only affects the internal calculations; values in x are always assumed to be logged, and
    the splicing index is always returned on the log2 scale.
}
\seealso{
  \code{\link{exonic}}
}
\author{
  Tim Yates
  Crispin J. Miller
}
\examples{
  if(interactive()) {
    # Loads the Expression Set into x.rma
    load( '../unitTests/HuEx-1_0.tp53.expr.RData' )
    spliceIndex( x.rma, symbolToGene( 'tp53' ), gps=list(1:3,4:6) )
  }
}
