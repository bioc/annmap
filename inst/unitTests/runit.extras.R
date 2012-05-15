# Testing for the utility functions of the package

if(FALSE) {
  library( "RUnit" )
  library( "annmap" )
}

###############################################################################
## SetUp is called before each test method
##
.setUp = function() {
}

###############################################################################
## TearDown is called after each test method
##
.tearDown = function() {
    annmapDisconnect()
}

test.filters = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    lapply( c( FALSE, TRUE ), function( cache ) {
      cat( paste( "Setting use.cache to", cache, "\n" ) )
      annmapSetParam( use.cache=cache )

      print( 'Fetch some data for p53' )
      gene = symbolToGene( "tp53" )

      probesets.vector = geneToProbeset( gene, as.vector=TRUE )
      probesets.frame  = geneToProbeset( gene )

      print( 'Check vector filtering' )
      exonic.vector     = exonic(     probesets.vector )
      intronic.vector   = intronic(   probesets.vector )
      unreliable.vector = unreliable( probesets.vector )
      intergenic.vector = intergenic( probesets.vector )

      print( 'Check RangedData filtering' )
      exonic.frame     = exonic(     probesets.frame )
      intronic.frame   = intronic(   probesets.frame )
      unreliable.frame = unreliable( probesets.frame )
      intergenic.frame = intergenic( probesets.frame )

      checkTrue( is.data.frame( exonic.frame ), "As source is a .data.frame, result should be one too" )
      checkTrue( is.data.frame( intronic.frame ), "As source is a .data.frame, result should be one too" )
      checkTrue( is.data.frame( unreliable.frame ), "As source is a .data.frame, result should be one too" )
      checkTrue( is.data.frame( intergenic.frame ), "As source is a .data.frame, result should be one too" )

      print( 'Check inverse filters' )
      exonic.vector.ex     = exonic(     probesets.vector, exclude=TRUE )
      intronic.vector.ex   = intronic(   probesets.vector, exclude=TRUE )
      unreliable.vector.ex = unreliable( probesets.vector, exclude=TRUE )
      intergenic.vector.ex = intergenic( probesets.vector, exclude=TRUE )

      exonic.frame.ex     = exonic(     probesets.frame, exclude=TRUE )
      intronic.frame.ex   = intronic(   probesets.frame, exclude=TRUE )
      unreliable.frame.ex = unreliable( probesets.frame, exclude=TRUE )
      intergenic.frame.ex = intergenic( probesets.frame, exclude=TRUE )

      print( 'Cross comparison checks' )
      checkTrue( length( c( exonic.vector, intronic.vector, unreliable.vector, intergenic.vector ) ) == length( probesets.vector ), "Split up, there should still be the same number of probesets" )
      checkTrue( all( dim( rbind( exonic.frame,  intronic.frame,  unreliable.frame,  intergenic.frame ) ) == dim( probesets.frame ) ), "Split up, there should still be the same number of probesets" )

      checkTrue( length( c( exonic.vector,     exonic.vector.ex ) )     == length( probesets.vector ), "exonic plus non-exonic should add up to all" )
      checkTrue( length( c( intronic.vector,   intronic.vector.ex ) )   == length( probesets.vector ), "intronic plus non-intronic should add up to all" )
      checkTrue( length( c( intergenic.vector, intergenic.vector.ex ) ) == length( probesets.vector ), "intergenic plus non-intergenic should add up to all" )
      checkTrue( length( c( unreliable.vector, unreliable.vector.ex ) ) == length( probesets.vector ), "unreliable plus non-unreliable should add up to all" )

      checkTrue( dim( rbind( exonic.frame,     exonic.frame.ex     ) )[1] == dim( probesets.frame )[1], "exonic plus non-exonic should add up to all" )
      checkTrue( dim( rbind( intronic.frame,   intronic.frame.ex   ) )[1] == dim( probesets.frame )[1], "intronic plus non-intronic should add up to all" )
      checkTrue( dim( rbind( intergenic.frame, intergenic.frame.ex ) )[1] == dim( probesets.frame )[1], "intergenic plus non-intergenic should add up to all" )
      checkTrue( dim( rbind( unreliable.frame, unreliable.frame.ex ) )[1] == dim( probesets.frame )[1], "unreliable plus non-unreliable should add up to all" )

      checkEquals( exonic.vector,     exonic.frame$stable_id )
      checkEquals( intronic.vector,   intronic.frame$stable_id )
      checkEquals( intergenic.vector, intergenic.frame$stable_id )
      checkEquals( unreliable.vector, unreliable.frame$stable_id )

      print( 'Check the has.probes.x filters' )
      probesets.vector.four = hasProbes( probesets.vector, num.probes=4, exclude=FALSE )
      probesets.frame.four = hasProbes( probesets.frame, num.probes=4, exclude=FALSE )
      checkTrue( is.data.frame( probesets.frame.four ), "As source is a data.frame, result should be one too" )
      checkTrue( length( probesets.vector.four ) <= length( probesets.vector ), "At most, there should be the same number of probesets as in the original call" )
      checkTrue( length( probesets.vector.four ) == dim( probesets.frame.four )[1], "Frame and dataset should have the same number of results" )

      probesets.vector.in = hasProbesIn( probesets.vector, num.probes=c( 0, 4 ), exclude=FALSE )
      probesets.frame.in = hasProbesIn( probesets.frame, num.probes=c( 0, 4 ), exclude=FALSE )
      checkTrue( is.data.frame( probesets.frame.in ), "As source is a data.frame, result should be one too" )
      checkTrue( length( probesets.vector.four ) == length( probesets.vector.in ), "Should have the same number of probesets as the has.probes call" )
      checkTrue( length( probesets.vector.in ) == dim( probesets.frame.in )[1], "Frame and dataset for .in should have the same number of results" )

      probesets.vector.least = hasProbesAtleast( probesets.vector, num.probes=4, exclude=FALSE )
      probesets.frame.least = hasProbesAtleast( probesets.frame, num.probes=4, exclude=FALSE )
      checkTrue( is.data.frame( probesets.frame.least ), "As source is a data.frame, result should be one too" )
      checkTrue( length( probesets.vector.least ) <= length( probesets.vector ), "At most, there should be the same number of probesets as in the original call for .atleast" )
      checkTrue( length( probesets.vector.least ) == dim( probesets.frame.least )[1], "Frame and dataset should have the same number of results for .atleast" )

      probesets.vector.between = hasProbesBetween( probesets.vector, min.probes=1, max.probes=4, exclude=FALSE )
      probesets.frame.between = hasProbesBetween( probesets.frame, min.probes=1, max.probes=4, exclude=FALSE )
      checkTrue( is.data.frame( probesets.frame.between ), "As source is a data.frame, result should be one too" )
      checkTrue( length( probesets.vector.between ) <= length( probesets.vector ), "At most, there should be the same number of probesets as in the original call for .atleast" )
      checkTrue( length( probesets.vector.between ) == dim( probesets.frame.between )[1], "Frame and dataset should have the same number of results for .atleast" )

      # Add a erroneous probeset name in and a duplicate, and check we still get the same number of rows back (the erroneous one should be NA)
      probesets.broken = probesets.vector
      probesets.broken[ 2 ] = "broken"
      probesets.broken[ 3 ] = probesets.broken[ 1 ]

      is.exonic.vector = isExonic( probesets.vector )
      is.exonic.broken = isExonic( probesets.broken )

      checkTrue( length( is.exonic.vector ) == length( is.exonic.broken ), "Should still et the same number of results" )
      checkTrue( !is.na( is.exonic.vector[ 2 ] ), "Second element should be TRUE or FALSE" )
      checkTrue(  is.na( is.exonic.broken[ 2 ] ), "Second element should be NA" )
      checkTrue(  names( is.exonic.broken )[ 1 ] == names( is.exonic.broken )[ 3 ], "First and third element should have the same name" )
      checkTrue(  is.exonic.broken[ 1 ] == is.exonic.broken[ 3 ], "First and third element should have the same value" )

      print( 'Edge case validation' )
      # Check that exonic for multiple identical exonic probesets returns us something reasonable
      exonic.rep = rep( exonic.vector[1], 5 )
      checkTrue( length( exonic( exonic.rep ) ) == 5, "Five go in, five should come out (irrespective of duplicates)" )

      # Check that we can filter out garbage probesets
      exonic.rep[ 3 ] = "garbage"
      checkTrue( length( exonic( exonic.rep ) ) == 4, "4 duplicates and one garbage probeset should return 4 exonic probesets" )

      probesets.broken = probesets.frame
      probesets.broken$stable_id[ 2 ] = "broken"
      probesets.broken[ 3, ] = probesets.broken[ 1, ]

      is.exonic.frame = isExonic( probesets.frame )
      is.exonic.broken = isExonic( probesets.broken )

      checkTrue( length( is.exonic.frame ) == length( is.exonic.broken ), "Should still et the same number of results" )
      checkTrue( !is.na( is.exonic.frame[ 2 ] ), "Second element should be TRUE or FALSE" )
      checkTrue(  is.na( is.exonic.broken[ 2 ] ), "Second element should be NA" )
      checkTrue(  names( is.exonic.broken )[ 1 ] == names( is.exonic.broken )[ 3 ], "First and third element should have the same name" )
      checkTrue(  is.exonic.broken[ 1 ] == is.exonic.broken[ 3 ], "First and third element should have the same value" )

      exonic.rep = rbind(exonic.frame[1,],exonic.frame[1,],exonic.frame[1,],exonic.frame[1,],exonic.frame[1,])
      checkTrue( length( exonic( exonic.rep )[,1] ) == 5, "Five go in (in a data.frame), five should come out (irrespective of duplicates)" )

      # Check that we can filter out garbage probesets
      exonic.rep$stable_id[ 3 ] = "garbage"
      checkTrue( length( exonic( exonic.rep )[,1] ) == 4, "4 duplicates and one garbage probeset (in a data.frame) should return 4 exonic probesets" )
    } )
    annmapDisconnect()
  }
}
