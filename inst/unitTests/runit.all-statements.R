# Testing all of the statements (to make sure none of them crash)
# Testing for acutal responses is more tricky, as the responses may change from one
# release to the next...

if(FALSE) {
  library( "RUnit" )
  library( "annmap" )
}

###############################################################################
## Utility methods
##
.get.functions = function( func.type, func.pattern, min.len, name.build.function ) {
  .functions = ls( envir=annmap:::.xmap.queries )
  .queries = strsplit( .functions[ grep( func.pattern, .functions ) ], '\\.' )
  checkTrue( length( .queries ) >= min.len, paste( "Should have at least ", min.len, " '", func.type, "' functions", sep="" ) )
  r = lapply( .queries, function(i) {
    name = name.build.function( i )
    c( name, getFunction( name ) )
  } )
  r
}

.run.full.test.suite = function() {
  1 == length( grep( 'picr.man.ac.uk', Sys.info()[ 'nodename' ] ) )
}

.run.functions = function( functions, executor ) {
  lapply( c( 'hs-test' ), function( db ) {
    data = tryCatch( annmapConnect( db ), error=function(e) { FALSE } )
    if( length( data ) == 1 ) {
      print( paste( "Cannot find datasource '", db, "', so skipping these tests.", sep="" ) )
    }
    else {
      # Set this to true if this species has no arrays
      no.arrays = is.null( allArrays( as.vector=F ) )
      lapply( functions, function( func ) {
        if( no.arrays && length( grep( "rray|robe|hit|Hit|Est|est|rediction", func[[1]] ) ) == 1 ) {
          print( paste( "Skipping", func[[1]], "on", annmap:::.xmap.internals$db.name ) )
        }
        else {
          print( paste( "Running", func[[1]], "on", annmap:::.xmap.internals$db.name ) )
          tryCatch( executor( func[[1]], func[[2]] ), error=function(e) {
            traceback()
            print( e )
          } )
        }
      } )
    }
  } )
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


test.gotsomefunctions = function() {
  checkTrue( length( ls( envir=annmap:::.xmap.queries ) ) >= 96, "Should have at least 96 functions" )
}

capitalise = function( name, initial=TRUE ) {
  name = sapply( name, function( s ) {
    s = strsplit( s, "_" )[[1]]
    s = paste( toupper( substring( s, 1, 1 ) ), substring( s, 2 ), sep="", collapse="" )
    if( !initial ) {
      s = paste( tolower( substring( s, 1, 1 ) ), substring( s, 2 ), sep="", collapse="" )
    }
    s
  } )
  name
}

test.all.queries = function() {
  if( .run.full.test.suite() ) {
    # Get all 'all' functions (of which there should be at least 14)
    .functions = .get.functions( 'all', '\\.all$', 14, function(i) { paste( 'all', capitalise( i[ 1 ] ), 's', sep="" ) } )
    # And run them, checking that we get at least one result from each
    .run.functions( .functions, function( name, f ) { r = f() ; checkTrue( length( r ) > 0, "Got no results!!" ) } )
  }
  else {
    print( '\nSkipping allXXX tests, as not on a PICR machine' )
  }
}