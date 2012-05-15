\name{annmapUtils}
\alias{annmapUtils}
\alias{annmapClearCache}
\alias{annmapConnect}
\alias{annmapDisconnect}
\alias{annmapAddConnection}
\alias{arrayType}
\alias{annmapToggleCaching}
\alias{annmapGenePlot}
\alias{annmapRangeApply}
\alias{strandAsInteger}
\title{
  annmap 'utils' functions
}
\description{
  Functions to connect to the database and manage the database connections. 
}
\usage{
  annmapConnect( name, use.webservice=FALSE )
  annmapDisconnect()
  annmapAddConnection( dsname, species, version,
                       host='localhost',
                       username=as.character( Sys.info()[ 'user' ] ),
                       password='',
                       port='',
                       overwrite=FALSE,
                       testConnect=TRUE )
  arrayType( name=NULL, pick.default=FALSE, silent=FALSE )
  annmapToggleCaching()
  annmapClearCache()
  annmapRangeApply( x, f, filter=c( chr="space", start="start", end="end", strand="strand" ), coerce=c( as.character, as.numeric, as.numeric, as.numeric ), ... )
  strandAsInteger( granges )
}
\arguments{
  \item{name}{ The name of the database to connect to, or the array to select. }
  \item{use.webservice}{ If \code{TRUE}, we will use the annmap webservices rather than a local MySQL installation.}
  \item{dsname}{ The name of the datasource to add or modify.}
  \item{species}{ The species of interest.}
  \item{version}{ The version of the database to connect to.}
  \item{host}{ The location of the MySQL installation.}
  \item{username}{ The username to connect to MySQL.}
  \item{password}{ The password required to connect to MySQL.}
  \item{port}{The port MySQL is running on. (Use NA for default)}
  \item{overwrite}{If another connection with this dsname already exists, should it be overwritten?}
  \item{testConnect}{If \code{TRUE}, the connection will be attempted before adding it to the databases.txt file.}
  \item{pick.default}{ If \code{TRUE}, \code{arrayType} will choose the first available arraytype for this species. }
  \item{silent}{ If \code{TRUE}, it will skip telling you which array you have selected. }
  \item{x}{A \code{\link{RangedData}} object}
  \item{f}{A function to apply to each \'row\' of the \code{\link{RangedData}} object}
  \item{filter}{Which \'columns\' of the \code{\link{RangedData}} object does the function need, and what parameters in the function do they map on to?. For example, by default, the field \'space\' gets mapped to the parameter \'chr\'.}
  \item{coerce}{What is the type of each parameter in \'f\'?}
  \item{...}{additional parameters to \'f\'}
  \item{granges}{A GRanges object}
}
\details{
  \code{annmapConnect} is used to establish a connection to an
  instance of the Annmap database, and  \code{annmapDisconnect} closes the connection.

  \code{arrayType} is used to specify the array you wish to use for queries based on Affymetrix probesets.

  Many of the functions in \code{annmap} cache results locally. The
  function \code{annmapToggleCaching} turns this functionality on and
  off, and  \code{annmapClearCache} can be used to clear the cache
  (this is not normally something a user needs to do).

  Note that details of how to set up the default databases, connection details, etc. Can be found in the package vignette.

  The function \code{strandAsInteger} takes a \code{GRanges} object and returns an integer vector of strands in the
  Ensembl style.  "+" becomes 1, "-" becomes -1, and "*" becomes \code{NA}.
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
  Crispin J. Miller
}
\examples{
  if(interactive()) {
    annmapConnect()
    annmapToggleCaching()
    annmapToggleCaching()

    annmapRangeApply(symbolToGene("TP53",as.vector=FALSE),probeInRange)

    #NOTE: since the next function empties out the local cache, don't
    #run it unless you want to do this!
    #annmapClearCache()
 }
}
