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

test.huisun.ANNMAP48 = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    checkTrue( length( proteinCoordsToGenome(protein.ids="ENSP00000369050", position=1, as.vector=TRUE, check.bounds=TRUE, truncate=TRUE) ) == 1, "Should return a single position" )
  }
}

test.ANNMAP112 = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    checkTrue( length( transcriptCoordsToGenome( transcriptDetails( 'ENST00000280979' ) ) ) == 1, "Transcript coords failing to convert to GRanges" )
    checkTrue( length( proteinCoordsToGenome( protein.ids="ENSP00000369050", position=1, as.vector=FALSE, check.bounds=TRUE, truncate=TRUE) ) == 1, "Should return a single GRanges position" )
  }
}