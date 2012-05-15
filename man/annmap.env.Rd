\name{annmapEnv}
\alias{annmapEnv}
\alias{annmapGetParam}
\alias{annmapSetParam}
\title{
  annmap 'env' functions
}
\description{
  Functions to access internal parameters
}
\usage{
  annmapEnv()
  annmapGetParam( key )
  annmapSetParam( ... )
}
\arguments{
  \item{ ... }{ A list of key-value parameters you wish to set. }
  \item{key}{ The key for the value you want to return. }
}
\details{
  These functions allow some access to \code{annmap}\'s configuration data. They are included to help debug database connection issues, and are not normally needed.

  On connection, a default arraytype (Affymetrix Exon arrays, where available) is specfied for the probe
  mappings. \code{arrayType} allows a different type of array to
  be specfied. This included for future compatibility.

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
    annmapEnv()
    annmapGetParam( "debug" )
    annmapConnect()
    annmapSetParam( debug=TRUE)
    annmapConnect()
    annmapSetParam( debug=FALSE)
    annmapDisconnect()
  }
}
