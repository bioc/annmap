# Testing for the db functions of the package

if(FALSE) {
  library( "RUnit" )
  library( "annmap" )
}

###############################################################################
## Create a test environment
##
.test.env = new.env( hash=TRUE, parent=emptyenv() )
.conf.dir = ''
###############################################################################
## SetUp is called before each test method
##
.setUp = function() {
  # Call initialise
  annmap:::.initialise()
  .test.env$.conf.dir = annmapGetParam( 'conf.dir' )
  print( paste( 'Got conf.dir', .test.env$.conf.dir ) )
}

###############################################################################
## TearDown is called after each test method
##
.tearDown = function() {
  print( paste( 'Resetting conf.dir to', .test.env$.conf.dir ) )
  annmapSetParam( conf.dir=.test.env$.conf.dir )
  annmapDisconnect()
}

test.databases.txt = function() {
  if( Sys.getenv( "RCMDCHECK" ) == "FALSE" ) {
    path = file.path( getwd(), "..", "inst", "unitTests" )
  }
  else {
    path = system.file( package=pkg, "unitTests" )
  }
  .procs = annmapGetParam( 'procs' )
  print( 'Mocking annmapConnect()' )
  .procs$connect = function( name ) {
    f = annmapGetParam( 'conf.dir' )
    f = file.path( f, "databases.txt" )
    if( !file.exists( f ) ) {
      stop( paste( "Can't find database config file '", f, "'. See package installation instructions for help setting this up.", sep="" ) )
    }
    a = read.table( f, sep="\t", strip.white=TRUE, header=TRUE )
    if( !all( colnames( a ) == c( "name", "host", "species", "version", "port", "username", "password" ) ) ) {
      a = read.table( f, sep=",", strip.white=TRUE, header=TRUE )
    }
    if( !all( colnames( a ) == c( "name", "host", "species", "version", "port", "username", "password" ) ) ) {
      stop( paste( "Unexpected column names in",  f, " - should be 'name', 'host', 'species', 'version', 'port', 'username' and 'password'" ) )
    }
    dbs = a[ ( as.character( a$name ) == name ), , drop=FALSE ]
    if( dim( dbs )[1] == 0 ) { stop( paste( "Invalid db name:", name ) ) }
    as.character( dbs[,"host"] )
  }
  annmapSetParam( procs=.procs )
  print( paste( 'setting conf.dir to', file.path( path, 'tabConfig' ) ) )
  annmapSetParam( conf.dir = file.path( path, 'tabConfig' ) )
  checkEquals( annmapConnect( 'testdb' ), 'localhost', 'Should connect to localhost with tab separated config' )
  print( paste( 'setting conf.dir to', file.path( path, 'commaConfig' ) ) )
  annmapSetParam( conf.dir = file.path( path, 'commaConfig' ) )
  checkEquals( annmapConnect( 'testdb' ), 'localhost', 'Should connect to localhost with comma separated config' )

  print( 'restoring connect method' )
  .procs$connect = annmap:::.xmc.connect
  annmapSetParam( procs=.procs )
}

test.connection = function() {
  checkEquals( annmapGetParam( "connected" ), FALSE, "Start not connected to anything" )
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    checkTrue( annmapGetParam( "connected" ), "Should now be connected" )
    annmapDisconnect()
    checkTrue( !annmapGetParam( "connected" ), "And connected status should be FALSE" )
  }
}

test.buildsql = function() {
  test.hash = new.env( hash=TRUE )
  test.hash$a = "p1"
  test.hash$b = "p2"
  test.hash$c = "p3"
  sql = annmap:::.build.sql2( "Parameter replacement for ${a}, ${b} and ${c} should be ok", test.hash )
  checkEquals( sql, paste( "Parameter replacement for ", test.hash$a, ", ", test.hash$b, " and ", test.hash$c, " should be ok", sep="" ) )
}

test.addConnection = function() {
  # Check if hs-test exists (set returned values into a DB var)
  db = tryCatch( annmapConnect( "hs-test" ), error=function(e) { NULL } )
  # disconnect
  annmapDisconnect()

  tmp = '/tmp'
  if( file.exists( tmp ) ) {
    # Set ANNMAP_HOME to /tmp
    annmapSetParam( conf.dir=tmp )

    # remove databases.txt if it exists
    databases.txt = file.path( annmapGetParam( 'conf.dir' ), 'databases.txt' ) 
    unlink( databases.txt )

    checkValues = function( test, name, species, version, username='', password='', host='localhost', port='' ) {
      get.row = function() {
        tab = annmap:::.read.databases( databases.txt, stringsAsFactors=FALSE )
        tab[ tab$name == name, ]
      }
      .row = get.row()
      for( field in tail( names( formals() ), n=-1 ) ) {
        a = .row[[ field ]]
        b = get( field )
        checkTrue( all.equal( a, b ),
                   paste( field, "differs -- Test", test, "|", a, class( a ), "!=", b, class( b ) ) )
      }
    }

    # add a new connection (testConnect=FALSE, as there is no db)
    annmapAddConnection( 'a', 'fish', '64', username='testing', testConnect=FALSE )
    # load databases.txt and check all fields are there
    checkValues( 'test1', 'a', 'fish', '64', username='testing' )

    # add a new connection (testConnect=FALSE, as there is no db)
    annmapAddConnection( 'b', 'fash', '64', username='testing', testConnect=FALSE )
    # Check again
    checkValues( 'test2', 'a', 'fish', '64', username='testing' )
    checkValues( 'test2', 'b', 'fash', '64', username='testing' )

    # update a connection with overwrite=TRUE
    annmapAddConnection( 'a', 'goose', '64', username='testing', testConnect=FALSE, overwrite=TRUE )
    # Check again
    checkValues( 'test3', 'a', 'goose', '64', username='testing' )
    checkValues( 'test3', 'b', 'fash', '64', username='testing' )

    # if !DB var exists GOTO last line
    if( !is.null( db ) ) {
      # add a new connection with the DB var's details (this will test connection)
      annmapAddConnection( 'working', db$species, db$version, host=db$host, username=db$username, password=db$password, port=db$port )
      # Check again
      checkValues( 'test4', 'a', 'goose', '64', username='testing' )
      checkValues( 'test4', 'b', 'fash', '64', username='testing' )
      checkValues( 'test4', 'working', db$species, db$version, username=db$username, password=db$password, host=db$host, port=db$port )
      # Try again with port set to the default
      annmapAddConnection( 'working', db$species, db$version, host=db$host, username=db$username, password=db$password, port='3306', overwrite=TRUE )
      # Check again
      checkValues( 'test4', 'a', 'goose', '64', username='testing' )
      checkValues( 'test4', 'b', 'fash', '64', username='testing' )
      checkValues( 'test4', 'working', db$species, db$version, username=db$username, password=db$password, host=db$host, port='3306' )
      # And make sure we fail when something ropey is added
      checkTrue( tryCatch( annmapAddConnection( 'working', db$species, db$version, host='idontexist', username=db$username, password=db$password, port='3306', overwrite=TRUE ),
                    error=function( e ) { TRUE } ), "Should have failed to add connection to idontexist server" )
      # Make sure our old settings remain..
      checkValues( 'test4', 'a', 'goose', '64', username='testing' )
      checkValues( 'test4', 'b', 'fash', '64', username='testing' )
      checkValues( 'test4', 'working', db$species, db$version, username=db$username, password=db$password, host=db$host, port='3306' )
    }
    # remove databases.txt
    unlink( databases.txt )
  }
}