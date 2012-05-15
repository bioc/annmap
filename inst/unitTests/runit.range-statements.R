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

test.range.queries = function() {
  # Get all 'range' functions (of which there should be at least 9)
  .functions = .get.functions( 'range', '\\.range$', 9, function(i) { paste( capitalise( i[1], F ), 'InRange', sep="" ) } )

  # And run them (searching chromosome 1 for 1000 bases on the forward strand)
    .run.functions( .functions, function( name, f ) { f( '1', 1000, 2000, 1 ) } )
}

test.multi.queries = function() {
  data = tryCatch( annmapConnect( 'hs-test' ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    tp53 = geneDetails( symbolToGene( 'tp53' ) )
    shh  = geneDetails( symbolToGene( 'shh' ) )

    .chrf = if( annmap:::.usegranges() ) seqnames else space

    loc = data.frame( chr   = c( as.character( annmapGetParam( 'spacefn' )( tp53 ) ), as.character( annmapGetParam( 'spacefn' )( shh ) ) ),
                      start = c( as.integer(   start( tp53 ) ), as.integer(   start( shh ) ) ),
                      end   = c( as.integer(     end( tp53 ) ), as.integer(     end( shh ) ) ),
                      strand= c( strandAsInteger(     tp53   ), strandAsInteger(     shh   ) ) )

    # ok, try with vectors for params
    checkTrue( length( geneInRange( loc$chr, loc$start, loc$end, loc$strand ) ) >= 2,      'Should be at least 2 genes returned for vector range query' )
    checkTrue( annmap:::.attr( tp53, 'stable_id' ) %in% geneInRange( loc$chr, loc$start, loc$end, loc$strand, as.vector=T ), 'TP53 should be in range of itself and others' )
    checkTrue( annmap:::.attr( tp53, 'stable_id' ) %in% geneInRange( loc$chr, loc$start, loc$end, loc$strand, as.vector=T ),  'SHH should be in range of itself and others' )

    # Test return types
    checkTrue( is.vector( geneInRange( loc$chr, loc$start, loc$end, loc$strand, as.vector=T ) ),                'as.vector=T should return vector' )
    checkTrue( class( geneInRange( loc$chr, loc$start, loc$end, loc$strand, as.vector=F ) )[1] == annmapGetParam( 'defaultclass' ),  paste( 'as.vector=F should return', annmapGetParam( 'defaultclass' ) ) )
    checkTrue( is.data.frame( geneInRange( loc$chr, loc$start, loc$end, loc$strand, as.vector='data.frame' ) ), 'as.vector="data.frame" should return data.frame' )

    # Try with a data.frame as the x param
    checkTrue( length( geneInRange( loc, as.vector=T ) ) >= 2,      'Should be at least 2 genes returned for data.frame range query' )
    checkTrue( annmap:::.attr( tp53, 'stable_id' ) %in% geneInRange( loc, as.vector=T ), 'TP53 should be in range of itself and others with data.frame' )
    checkTrue( annmap:::.attr( shh,  'stable_id' ) %in% geneInRange( loc, as.vector=T ),  'SHH should be in range of itself and others with data.frame' )

    # Try with a RangedData object as the x param
    colnames(loc)[colnames(loc) == 'chr'] = 'space'
    rd.loc = as( loc, 'RangedData' )
    checkTrue( length( geneInRange( rd.loc, as.vector=T ) ) >= 2,      'Should be at least 2 genes returned for RangedData range query' )
    checkTrue( annmap:::.attr( tp53, 'stable_id' ) %in% geneInRange( rd.loc, as.vector=T ), 'TP53 should be in range of itself and others with RangedData' )
    checkTrue( annmap:::.attr( shh,  'stable_id' ) %in% geneInRange( rd.loc, as.vector=T ),  'SHH should be in range of itself and others with RangedData' )
  }
}
