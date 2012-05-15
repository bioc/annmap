spliceGroupIndex = function( x, group.column, members ) {
  # extract the meta-data from the eSet object
  pd  = pData( x )
  # Find the column of interest
  grp = pd[, colnames( pd ) == group.column ]
  # Return the indices of interest
  seq_along( grp )[ is.element( grp, members ) ]
}

spliceIndex = function( x, ids, group, gps, group.index.fn=spliceGroupIndex, median.gene=FALSE, median.probeset=FALSE, unlogged=TRUE ) {
  tr1 = if( missing( group ) ) { gps[[1]] } else { group.index.fn(x, group, gps[1]) }
  tr2 = if( missing( group ) ) { gps[[2]] } else { group.index.fn(x, group, gps[2]) }
  r = geneToExonProbesetExpr( x, ids )
  if( dim( r )[ 1 ] == 0 ) {
    # No results
    return( r )
  }
  # Split based on gene id
  gene.list = split( r, r$IN1 )
  lapply( gene.list, function( a ) {
    a   = a[ !duplicated( a$probeset ), , drop = FALSE ]
    idx = c( tr1, tr2 )
    tmp = as.matrix( a[, idx, drop = FALSE ] )
    tr1 = seq_along( tr1 )
    tr2 = length( tr1 ) + seq_along( tr2 )
    idx = c( tr1, tr2 )
    # Get our gene and probeset average function dependant on params
    avfun.g = if( median.gene     ) { median } else { mean }
    avfun.p = if( median.probeset ) { median } else { mean }
    # Generate the SI depending on whether the expression data was logged or not
    tmp = if( unlogged ) { 2^tmp } else { tmp }
    avs = apply( tmp, 2, avfun.g )
    tmp = sweep( tmp, 2, avs, if( unlogged ) { '/' } else { '-' } )
    s1  = apply( tmp[, tr1, drop = FALSE], 1, avfun.p )
    s2  = apply( tmp[, tr2, drop = FALSE], 1, avfun.p )
    si  = if( unlogged ) { log2( s1 / s2 ) } else { s1 - s2 }
    fac = as.factor( idx %in% tr2 )
    r   = rowttests( tmp, fac )
    val.and.stats = r[, c( 'p.value', 'statistic' ),drop = FALSE]
    repeated.avg  = rep( avfun.g( log2( avs[ tr1 ] ) - log2( avs[ tr2 ] ) ), length( si ) )
    r             = cbind( si, val.and.stats, repeated.avg )
    # Set the row an column names for the returned object
    rownames( r ) = a$probeset
    colnames( r ) = c( 'si', 'p.score', 't.statistic', 'gene.av' )
    r
  } )
}