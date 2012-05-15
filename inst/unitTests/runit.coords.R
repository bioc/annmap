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