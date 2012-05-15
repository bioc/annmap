# Extra functions that are not generated automagically from the schema

.generate.stats.bits = function( probesets=NULL ) {
  if( is.null( probesets ) ) {
    probesets = allProbesets( as.vector=FALSE )
  }
  else {
    probesets = probesetDetails( probesets )
  }
  if( is.null( probesets ) ) {
    bits = NULL
  }
  else {
    bits = data.frame( probesets[,'probe_count'],
                   ( ( probesets[,'hit_score'] == 1 ) & ( probesets[,'gene_score'] >  0 ) & ( probesets[,'exon_score'] >  0 ) ) + # exonic
                   2 * ( ( probesets[,'hit_score'] == 1 ) & ( probesets[,'gene_score'] >  0 ) & ( probesets[,'exon_score'] == 0 ) ) + # intronic
                 4 * ( ( probesets[,'hit_score'] == 1 ) & ( probesets[,'gene_score'] == 0 ) & ( probesets[,'exon_score'] == 0 ) ) + # intergenic
                 8 * ( ( probesets[,'hit_score'] > 1 ) | ( probesets[,'hit_score'] == 0 ) ),                                        # unreliable
           row.names=probesets[,'stable_id'] )
    colnames( bits ) = c( 'probe_count', 'scores' )
  }
  bits
}

.get.stats.cache = function( probeset.names=NULL ) {
  bits = NULL
  if( is.null( .xmap.internals$connected ) || !.xmap.internals$connected ) {
    stop( "You need to connect to a database. -- see annmapConnect()")
  }
  if( .xmap.internals$use.cache ) {
    .xmap.internals$debugFn( "Loading specificity cache" )
    key = paste( .xmap.internals$array, .xmap.internals$version, .xmap.internals$species, ".probeset.cache", sep=':' )
    bits = .cache.retrieve( key )
    .xmap.internals$debugFn( "Done..." )
    if( is.null( bits ) ) {
      cat( "Building probeset specificity cache..." )
      flush.console()
      bits = .generate.stats.bits()
      .cache.store( key, bits )
      cat( "...done\n" )
    }
    .xmap.internals$debugFn( c( "Filtering specificity cache down to", length( probeset.names ), "required probesets" ) )
    bits = bits[ rownames( bits ) %in% probeset.names,, drop=F ]
    .xmap.internals$debugFn( "...done" )
  }
  else {
    bits = .generate.stats.bits( probeset.names )
  }
  bits
}

.score.filter = function( probesets, column.name, score.function ) {
    # Get a vector of probeset names
  probesets = .get.correct.column( 'probeset', probesets )

  # Get the stats for these probesets (if cache is off) or all probesets
  stats = .get.stats.cache( unique( probesets ) )

    if( is.null( stats ) ) {
    ret = rep( NA, length( probesets ) )
  }
  else {
    # Generate a truth table based on the score
    .xmap.internals$debugFn( c( "Generating truth table for stats dim=", dim(stats) ) )
    ret = score.function( stats[ probesets, column.name ] )
    .xmap.internals$debugFn( "...done" )
  }

  # And set the names of the truth table values
  names( ret ) = probesets
  ret
}

isExonic = function( probesets ) {
  .score.filter( probesets, 'scores', function( a ) a == 1 )
}

isIntronic = function( probesets ) {
  .score.filter( probesets, 'scores', function( a ) a == 2 )
}

isIntergenic = function( probesets ) {
  .score.filter( probesets, 'scores', function( a ) a == 4 )
}

isUnreliable = function( probesets ) {
  .score.filter( probesets, 'scores', function( a ) a == 8 )
}

.filter.whatever = function( probesets, score.function, exclude ) {
  r = score.function( probesets )
  if( is.data.frame( probesets ) ) {
    probesets = probesets[ xor( !is.na( r ) & r, exclude ), ]
  }
  else {
    probesets = probesets[ xor( !is.na( r ) & r, exclude ) ]
  }
  if( length( probesets ) == 0 ) {
    probesets = NULL
  }
  probesets
}

exonic = function( probesets, exclude=FALSE ) {
  .filter.whatever( probesets, isExonic, exclude )
}

intronic = function( probesets, exclude=FALSE ) {
  .filter.whatever( probesets, isIntronic, exclude )
}

intergenic = function( probesets, exclude=FALSE ) {
  .filter.whatever( probesets, isIntergenic, exclude )
}

unreliable = function( probesets, exclude=FALSE ) {
  .filter.whatever( probesets, isUnreliable, exclude )
}

hasProbes = function( probesets, num.probes=4, exclude=FALSE ) {
  .filter.whatever( probesets, function( a ) .score.filter( a, 'probe_count', function( b ) b == num.probes ), exclude )
}

hasProbesAtleast = function( probesets, num.probes=4, exclude=FALSE ) {
  .filter.whatever( probesets, function( a ) .score.filter( a, 'probe_count', function( b ) b >= num.probes ), exclude )
}

hasProbesIn = function( probesets, num.probes=c( 1, 2, 3, 4 ), exclude=FALSE ) {
  .filter.whatever( probesets, function( a ) .score.filter( a, 'probe_count', function( b ) b %in% num.probes ), exclude )
}

hasProbesBetween = function( probesets, min.probes=1, max.probes=4, exclude=FALSE, inclusive=TRUE ) {
  fn = if( inclusive ) { 
    function( b ) { ( b >= min.probes ) & ( b <= max.probes ) }
  }
  else {
    function( b ) { ( b > min.probes ) & ( b < max.probes ) }
  }
  .filter.whatever( probesets, function( a ) .score.filter( a, 'probe_count', fn ), exclude )
}