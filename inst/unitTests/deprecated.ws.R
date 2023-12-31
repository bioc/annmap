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

.run.full.test.suite = function() {
  1 == length( grep( 'picr.man.ac.uk', Sys.info()[ 'nodename' ] ) )
}

test.ANNMAP33 = function() {
  if( !capabilities()["http/ftp"] ) {
    print( '\nSkipping webservice tests, no http/ftp capabilities' )
    return()
  }
  a = tryCatch( url( 'http://annmap.cruk.manchester.ac.uk', 'r' ), warning=function( w ) NULL, error=function( e ) NULL )
  if( is.null( a ) ) {
    print( '\nSkipping webservice tests, no route to annmap website' )
    return()
  }
  close( a )
  if( require( 'rjson', quietly=T ) ) {
    db = paste( 'homo_sapiens', fromJSON( file='http://annmap.cruk.manchester.ac.uk/data/init.js' )$items$homo_sapiens$currentVersion, sep='.' )
    annmapConnect( db, use.webservice=T )
    annmapSetParam( debug=T )
    exons = geneToExon( symbolToGene( 'tp53' ), as.vector=TRUE )
    checkTrue( length( exons ) > 1, 'Expected more than one exon from geneToExon via webservice' )
    annmapDisconnect()
  }
}

test.connection = function() {
  if( !capabilities()["http/ftp"] ) {
    print( '\nSkipping webservice tests, no http/ftp capabilities' )
    return()
  }

  a = tryCatch( url( 'http://annmap.cruk.manchester.ac.uk', 'r' ), warning=function( w ) NULL, error=function( e ) NULL )
  if( is.null( a ) ) {
    print( '\nSkipping webservice tests, no route to annmap website' )
    return()
  }
  close( a )

  if( require( 'rjson', quietly=T ) ) {
    db = paste( 'homo_sapiens', fromJSON( file='http://annmap.cruk.manchester.ac.uk/data/init.js' )$items$homo_sapiens$currentVersion, sep='.' )
    annmapConnect( db, use.webservice=T )

    chrs = chromosomeDetails( allChromosomes() )
    checkTrue( length( chrs ) == 25, 'Should have got 25 rows via chromosomeDetails via WS' )

    annmapDisconnect()
  }
}

