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
  lapply( c( 'hs-test', 'mm-test', 'rn-test', 'pb-test' ), function( db ) {
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

test.gotsomefunctions = function() {
  checkTrue( length( ls( envir=annmap:::.xmap.queries ) ) >= 96, "Should have at least 96 functions" )
}

test.transcript.to.translatedprobes = function() {
  data = tryCatch( annmapConnect( 'hs-test' ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( paste( "Cannot find datasource 'hs-test', so skipping these tests.", sep="" ) )
  }
  else {
    transcripts = geneToTranscript( symbolToGene( 'TP53' ), as.vector=TRUE )
    checkTrue( length( transcriptToTranslatedprobes( transcripts ) ) == length( transcripts ), "vector transcriptToTranslatedprobes lengths differ" )
    transcripts = geneToTranscript( symbolToGene( 'TP53' ) )
    if( annmap:::.usegranges() ) {
      checkTrue( length( transcriptToTranslatedprobes( transcripts ) ) == length( transcripts ), "GRanges transcriptToTranslatedprobes lengths differ" )
    }
    else {
      checkTrue( length( transcriptToTranslatedprobes( transcripts )[[1]] ) == length( transcripts ), "RangedData transcriptToTranslatedprobes lengths differ" )
    }

    # Check that bad transcript names doesn't crash
    bad = transcriptToTranslatedprobes( 'bad' )
    checkTrue( length( bad ) == 1,         "Should still return a single item" )
    checkTrue( names( bad ) == c( 'bad' ), "Should have the original name" )
    checkTrue( is.null( bad$bad ),         "And it should be null" )

    # Check that bad transcript names doesn't crash
    transcripts = c( 'ENST00000455263', 'bad', 'ENST00000414315' )
    bad = transcriptToTranslatedprobes( transcripts )
    checkTrue( length( bad ) == length( transcripts ), "Should return same number of objects" )
    checkTrue( all( names( bad ) == transcripts ),     "Names should remain the same, in the same order" )
    checkTrue( is.null( bad$bad ),                     "And the middle one should be null" )
  }
}

test.to.queries = function() {
  # Get all 'to' functions (of which there should be at least 58)
  .functions = .get.functions( 'to', '\\.to\\.', 58, function(i) { paste( capitalise( i[1], F ), 'To', capitalise( i[3] ), sep="" ) } )

  # And run them, searching for the thing called 'InvalidId' which should return no results, but not crash)
    .run.functions( .functions,  function( name, f ) {
    if( !.run.full.test.suite() && name == "arrayToProbeset" ) {
      print( '\nSkipping array.to.probeset test, as not on a PICR machine' )
    }
    else {
      r = f( 'InvalidId' )
      if( name == "arrayToProbeset" ) {
        checkTrue( length( r ) > 0, "should have some probesets for an array" )
      }
      else {
        checkTrue( is.null( r ), "should return null" )
      }
    }
  } )
}

test.expr.query = function() {
  data = tryCatch( annmapConnect( 'hs-test' ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( paste( "Cannot find datasource 'hs-test', so skipping these tests.", sep="" ) )
  }
  else {
    if( Sys.getenv( "RCMDCHECK" ) == "FALSE" ) {
      path = file.path( getwd(), "..", "inst", "rdata" )
    }
    else {
      path = system.file( package=pkg, "rdata" )
    }
    lapply( allArrays( as.vector=TRUE ), function( arr ) {
      arrayType( arr )
      file.name = file.path( path, paste( arr, 'tp53.expr.RData', sep='.' ) )
      if( file.exists( file.name ) ) {
        env = new.env()
        print( paste( 'Loading', file.name ) )
        load( file.name, env )
        o.name = ls( env )[ 1 ]
        print( paste( 'Using variable', o.name ) )
        rslt = geneToExonProbesetExpr( get( o.name, envir=env ), symbolToGene( 'tp53' ) )
        rslt.dim = dim( rslt )
        checkTrue( rslt.dim[ 1 ] > 0, 'Should have rows in out exprs result data' )
        checkTrue( rslt.dim[ 2 ] > 0, 'Should have columns in out exprs result data' )
      }
      else {
        print( paste( '***WARN*** Cannot find file', file.name, 'to test gene.to.exon.probeset.expr for', arr ) )
      }
    } )
  }
}
