# Some sanity checks on the data returned from annmap

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

test.chaining = function() {
  data = tryCatch( annmapConnect( 'hs-test' ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( paste( "Cannot find datasource 'hs-test' so skipping this test." ) )
  }
  else {
    checkEquals( is.vector( geneToProbeset( symbolToGene( "tp53" ), as.vector=TRUE ) ),
             TRUE,
           "Should return a vector when as.vector is TRUE" )
    checkEquals( is.data.frame( geneToProbeset( symbolToGene( "tp53" ) ) ),
             TRUE,
           "as.vector == false (default) with probesets should return a data.frame" )
    checkEquals( class( probesetToHit( geneToProbeset( symbolToGene( "tp53" ) ) ) )[1],
           annmapGetParam( 'defaultclass' ),
           paste( "default with hits should return a", annmapGetParam( 'defaultclass' ), "object" ) )
    checkEquals( geneToProbeset( symbolToGene( "tp53", as.vector=TRUE ) ),
             geneToProbeset( symbolToGene( "tp53" ) ),
             paste( "Should get the same output no mater whether the input is a vector or", annmapGetParam( 'defaultclass' ) ) )
    annmapDisconnect()
  }
}
