.make.hash = function() {
  new.env( hash=TRUE, parent=emptyenv() )
}

# A global holding our internal state (as a hash, as hashes are mutable in R)
.xmap.internals = .make.hash()

annmapAddConnection = function( dsname, species, version,
                                host='localhost',
                                username=as.character( Sys.info()[ 'user' ] ),
                                password='',
                                port='',
                                overwrite=FALSE,
                                testConnect=TRUE ) {
  if( missing( dsname ) || missing( species ) || missing( version ) ) {
    stop( "Must specify a dsname, species and version" )
  }
  if( is.null( .xmap.internals$initialised ) ) {
    .initialise()
  }
  dsname = as.character( dsname )
  species = as.character( species )
  version = as.character( version )
  host = as.character( host )
  username = as.character( username )
  password = as.character( password )
  port = as.character( port )
  if( testConnect == TRUE ) {
    con = .get.connection( species, version, username, password, host, port )
    if( is.null( con$con ) ) {
      cat( message( con$err ) )
      cat( '\n' )
      stop( paste( 'Could not connect to database, tried', paste( c( 'annmap_', 'xmapcore_' ), species, '_', version, sep='', collapse=', ' ), 'on server', host ) )
    }
    dbDisconnect( con$con )
  }
  .files = c( .xmap.internals$conf.dir, file.path( .xmap.internals$conf.dir, "databases.txt" ) )
  .exists = file.exists( .files )
  .access = file.access( .files, mode=2 )
  if( .exists[ 1 ] == FALSE ) {
    stop( paste( "Cannot locate configuration directory", .files[ 1 ] ) )
  }
  if( .access[ 1 ] != 0 ) {
    stop( paste( "No write access to configuration directory", .files[ 1 ] ) )
  }
  if( .exists[ 2 ] == TRUE && .access[ 2 ] != 0 ) {
    stop( paste( "No write access to configuration file", .files[ 2 ] ) )
  }
  .method = 'added.'
  .db = if( .exists[ 2 ] == FALSE ) {
    # Make a new data.frame
    data.frame( name=dsname, host=host, species=species, version=version, port=port, username=username, password=password )
  }
  else {
    # Load existing and append
    db = .read.databases( .files[ 2 ], stringsAsFactors=FALSE )
    if( dsname %in% db[,'name'] ) {
      if( overwrite == FALSE ) {
        stop( paste( "A connection with the name", dsname, "already exists" ) )
      }
      .method = 'updated.'
      db[ db$name == dsname, ] = list( name=dsname, host=host, species=species, version=version, port=port, username=username, password=password )
    }
    else {
      db = rbind( db, list( name=dsname, host=host, species=species, version=version, port=port, username=username, password=password ) )
    }
    db
  }
  cat( paste( 'Connection', dsname, .method, '\n' ) )
  # Then write this back out...
  write.table( .db, .files[ 2 ], quote=FALSE, sep=',', row.names=FALSE )
  invisible( list( host=host, species=species, version=version, port=port, username=username, password=password ) )
}

# Exported functions
annmapConnect = function( name, use.webservice=FALSE ) {
  if( ( use.webservice  &&  is.null( .xmap.internals$webservice ) ) ||
      ( !use.webservice && !is.null( .xmap.internals$webservice ) ) ||
      ( is.null( .xmap.internals$initialised ) ) ) {
    .initialise()
  }
  if( use.webservice ) {
    if( !capabilities()["http/ftp"] ) {
      stop( 'The version of R you are using is not built with http capability.' )
    }
    a = tryCatch( url( 'http://annmap.picr.man.ac.uk', 'r' ), warning=function( w ) NULL, error=function( e ) NULL )
    if( is.null( a ) ) {
      stop( 'Cannot connect to http://annmap.picr.man.ac.uk. Are you sure you have an internet connection?' )
    }
    close( a )
    if( !require( 'rjson', quietly=T ) ) { 
      message( 'Package rjson is required for webservice connection.' )
      if( interactive() ) {
        message( 'Attempting to install' )
        install.packages( 'rjson' )
        message( 'Ok, trying to load rjson again.' )
        require( 'rjson' )
      }
      else {
        stop( 'Please install rjson and try again.' )
      }
    }
    .param.cache = annmapGetParam( "procs" )
    .procs = .make.hash()
    .procs$all = .xmcws.all
    .procs$details = .xmcws.details
    .procs$to = .xmcws.to
    .procs$range = .xmcws.range
    .procs$params = .ws.make.params
    .procs$connect = .xmcws.connect
    .procs$disconnect = .xmcws.disconnect
    annmapSetParam( procs=.procs )
    annmapSetParam( webservice=TRUE )
    annmapGetParam( "debugFn" )( "Using WebService for data access.\n" )
  }
  else {
    annmapSetParam( webservice=NULL )
  }
  .xmap.internals$procs$connect( name )
}

annmapDisconnect = function() {
  .xmap.internals$procs$disconnect()
}

.read.databases = function( f, ... ) {
  a = tryCatch( read.table( f, sep="\t", strip.white=TRUE, header=TRUE, fill=TRUE, colClasses='character', ... ), error=function(e) { data.frame( bad=TRUE ) } )
  if( !all( colnames( a ) == c( "name", "host", "species", "version", "port", "username", "password" ) ) ) {
    a = read.table( f, sep=",", strip.white=TRUE, header=TRUE, fill=TRUE, colClasses='character', ... )
  }
  if( !all( colnames( a ) == c( "name", "host", "species", "version", "port", "username", "password" ) ) ) {
    stop( paste( "Unexpected column names in", 
            f,
            " - should be 'name', 'host', 'species', 'version', 'port', 'username' and 'password'" ) )
  }
  a
}

.get.connection = function( species, version, un, pw, host, port, db.prefixes=c( 'annmap_', 'xmapcore_' ) ) {
  for( i in 1:length( db.prefixes ) ) {
    db.name = paste( db.prefixes[ i ], species, "_", version, sep="" )
    if( is.na( port ) || is.null( port ) || port == '' ) {
      con = tryCatch( dbConnect( MySQL(), user=un, password=pw, dbname=db.name, host=host, client.flag=131072 ), error=function(e) e )
    }
    else {
      con = tryCatch( dbConnect( MySQL(), user=un, password=pw, dbname=db.name, host=host, port=as.numeric( port ), client.flag=131072 ), error=function(e) e )
    }
    if( !is.null( con ) && all( class( con ) != 'simpleError' ) ) {
      return( list( con=con, db.name=db.name, err=NULL ) )
    }
  }
  list( con=NULL, db.name=NULL, err=con )
}

# Internal functions
.xmc.connect = function( name ) {
  f = .xmap.internals$conf.dir
  f = file.path( f, "databases.txt" )
  if( !file.exists( f ) ) {
    stop( paste( "Can't find database config file '",
            f,
            "'. See package installation instructions for help setting this up.",
            sep="" ) )
  }
  a = .read.databases( f )
  if( missing( name ) ) {
    r = 1
    if( dim( a )[1] > 1 ) {
      choices = paste( as.character( a[,1] ), " -- ", as.character( a[,3] ), " v", as.character( a[,4] ), " (", as.character( a[,2] ), ")", sep="" )
      r = menu( choices, title="Select a database to connect to:" )
    }
    if( r == 0 ) {
      return( invisible() )
    }
    else {
      name = a[,1][r]
    }
  }
  dbs = a[ ( as.character( a$name ) == name ), , drop=FALSE ]
  if( dim( dbs )[1] == 0 ) {
    stop( paste( "Invalid db name:", name ) )
  }
  un = .get.user.string( as.character( dbs[,"username"] ), "username" )
  if( is.na( dbs[,"password"] ) ) {
    pw = ""
  }
  else {
    pw = .get.user.string( as.character( dbs[,"password"] ), "password" )
  }
  annmapDisconnect()
  species = as.character( dbs[,"species"] )
  version = as.character( dbs[,"version"] )
  host = as.character( dbs[,"host"] )
  port = as.character( dbs[,"port"] )
  con = .get.connection( species, version, un, pw, host, port )
  if( is.null( con$con ) ) {
    cat( message( con$err ) )
    cat( '\n' )
    stop( paste( 'Could not connect to database, tried', paste( c( 'annmap_', 'xmapcore_' ), species, '_', version, sep='', collapse=', ' ), 'on server', host ) )
  }
  .xmap.internals$con       = con$con
  .xmap.internals$species   = species
  .xmap.internals$version   = version
  .xmap.internals$host      = host
  .xmap.internals$port      = port
  .xmap.internals$db.name   = con$db.name
  .xmap.internals$connected = TRUE

  cat( paste( 'Connected to ', .xmap.internals$db.name, ' (', .xmap.internals$host, ')\n', sep='' ) )

  arrayType( NULL, pick.default=TRUE )

  invisible( list( host=host, species=species, version=version, port=port, username=un, password=pw ) )
}

.xmc.disconnect = function() {
  if( !is.null( .xmap.internals$con ) ) {
    cat( paste( 'Disconnecting from ', .xmap.internals$db.name, ' (', .xmap.internals$host, ')\n', sep='' ) )
    dbDisconnect( .xmap.internals$con )
    tryCatch( rm( 'con',       envir=.xmap.internals ), warning=function(a){invisible()} )
    tryCatch( rm( 'species',   envir=.xmap.internals ), warning=function(a){invisible()} )
    tryCatch( rm( 'array',     envir=.xmap.internals ), warning=function(a){invisible()} )
    tryCatch( rm( 'db.name',   envir=.xmap.internals ), warning=function(a){invisible()} )
  }
  .xmap.internals$connected = FALSE
}

.xmc.db.cleanup = function( conn ) {
  res = dbListResults( conn )
  for( i in res )  {
    if( !dbHasCompleted( i ) ) {
      fetch( i, -1 )
      dbClearResult( i )
    }
  }
  while( dbMoreResults( conn ) ) {
    res = dbNextResult( conn )
    if( !dbHasCompleted( res ) ) {
      fetch( res, -1 )
      dbClearResult( res )
    }
  }
}

.xmc.db.call = function( sql, conn ) {
  .xmap.internals$debugFn( sql )
  on.exit( .xmc.db.cleanup( conn ) )
  res  = dbSendQuery( conn, sql )
  d    = fetch( res, -1 )
  if( length( d ) == 0 ) {
    d = NULL
  }
  d
}

.build.sql2 = function( stmt, hash ) {
  if( !is.null( hash ) ) {
    vars = ls( envir=hash )
    for( var in vars ) {
      val = get( var, envir=hash )
      patt = paste( "\\$\\{", var, "\\}", sep="" )
      stmt = gsub( patt, val, stmt )
    }
  }
  stmt
}

