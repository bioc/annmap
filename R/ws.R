.make.hash = function() {
  new.env( hash=TRUE, parent=emptyenv() )
}

.xmcws.connect = function( name ) {
  data = suppressWarnings( fromJSON( file='http://annmap.cruk.manchester.ac.uk/data/init.js' ) )
  a = do.call( 'rbind', unlist( lapply( names( data$items ), function( s ) {
    lapply( data$items[[ s ]]$versions, function( v ) {
      list( name=paste( s, v, sep='.' ), dbname=s, display=data$items[[ s ]]$display, version=v )
    } )
  } ), recursive=F ) )
  if( missing( name ) ) {
    r = menu( apply( a, 1, function( row ) {
      paste( row$name, ' -- ', row$display, ' v', row$version, ' (http://annmap.cruk.manchester.ac.uk/?s=', row$dbname, ')', sep='' )
    } ), title="Select a database to connect to:" )
    if( r == 0 ) {
      return( invisible() )
    }
    else {
      name = a[ r, 'name' ]
    }
  }
  dbs = a[ ( as.character( a[, 'name'] ) == name ), , drop=FALSE ]

  if( dim( dbs )[1] == 0 ) {
    stop( paste( "Invalid db name:", name ) )
  }
  annmapDisconnect()
  species = as.character( dbs[,"dbname"] )
  version = as.character( dbs[,"version"] )
  annmapSetParam( species=species, version=version, host='http://annmap.cruk.manchester.ac.uk', db.name=name, connected=TRUE )

  cat( paste( 'Connected to ', name, ' (http://annmap.cruk.manchester.ac.uk)\n', sep='' ) )

  arrayType( NULL, pick.default=TRUE )

  invisible( list( host='http://annmap.cruk.manchester.ac.uk', species=species, version=version ) )
}

.load.and.parse = function( elements ) {
  url = paste( 'http://annmap.cruk.manchester.ac.uk/data/annmapws/', paste( elements, collapse='/' ), '.js', sep='' )

  .xmap.internals$debugFn( paste( 'calling', url ) )

  data = suppressWarnings( fromJSON( file=url ) )

  if( !is.null( data$error ) ) {
    stop( data$error )
  }

  if( length( data$items ) == 0 ) {
    return( NULL )
  }
  else if( is.null( names( data$items ) ) ) { # list from .all query
    rslt = do.call( 'rbind', lapply( seq_along( data$items ), function( r ) {
      ret2 = data$items[[ r ]]
      ret2 = ret2[ !( names( ret2 ) %in% c( '__type', '__id', 'synonyms' ) ) ]
      ret2[ sapply( ret2, is.null ) ] = NA
      data.frame( ret2, stringsAsFactors=F )
    } ) )
  }
  else { # Map from other query
    rslt = do.call( 'rbind', lapply( names( data$items ), function( IN1 ) {
      ret = do.call( 'rbind', lapply( seq_along( data$items[[ IN1 ]] ), function( idx ) {
        ret2 = data$items[[ IN1 ]][[ idx ]]
        ret2$IN1 = IN1
        ret2 = ret2[ !( names( ret2 ) %in% c( '__type', '__id', 'synonyms' ) ) ]
        ret2[ sapply( ret2, is.null ) ] = NA
        data.frame( ret2, stringsAsFactors=F )
      } ) )
    } ) )
  }
  # type convert values which need it (start, end, strand)
  if( !is.null( rslt$start ) )  rslt$start  = as.integer( rslt$start )
  if( !is.null( rslt$end ) )    rslt$end    = as.integer( rslt$end )
  if( !is.null( rslt$strand ) ) rslt$strand = as.integer( rslt$strand )
  rslt
}

.xmcws.disconnect = function() {
  # Do nowt
}

.xmcws.all = function( src, params, dest=NULL ) {
  cache.id = paste( .xmap.internals$version, .xmap.internals$species, src, 'all', params$array, sep=':' )
  ret = .cache.retrieve( cache.id )
  if( is.null( ret ) ) {
    elements = c( .xmap.internals$species, .xmap.internals$version, src, 'all' )
    if( !is.null( params$array ) ) {
      elements = c( elements, params$array )
    }
    ret = .load.and.parse( elements )
    .cache.store( cache.id, ret )
  }
  ret
}

.xmcws.details = function( src, params, dest=NULL ) {
  if( is.null( params ) ) {
    return( NULL )
  }
  elements = c( annmapGetParam( 'species' ), annmapGetParam( 'version' ), src, 'details' )
  if( !is.null( params$array ) ) {
    elements = c( elements, params$array )
  }
  elements = c( elements, params$ids )
  .load.and.parse( elements )
}

.xmcws.to = function( src, params, dest=NULL ) {
  if( is.null( params ) ) {
    return( NULL )
  }
  elements = c( annmapGetParam( 'species' ), annmapGetParam( 'version' ), 'to', src, dest, if( !is.null( params$array ) ) params$array else 'null', params$ids )
  .load.and.parse( elements )
}

.xmcws.range = function( src, params, dest=NULL ) {
  if( is.null( params ) ) {
    return( NULL )
  }
  elements = c( annmapGetParam( 'species' ), annmapGetParam( 'version' ), src, 'range' )
  if( !is.null( params$array ) ) {
    elements = c( elements, params$array )
  }
  elements = c( elements, params$chr, format( params$qstart, scientific=F ), format( params$qstop, scientific=F ), params$qstrand )
  .load.and.parse( elements )
}

.ws.make.params = function( ids ) {
    if( is.null( ids ) ) {
        stop( "In .ws.make.params, ids is NULL" )
    }
    if( is.list( ids ) ) {
        ids = unlist( ids, use.names=FALSE )
    }
    mq        = 2000
    one       = max( nchar( ids ) ) + 1
    max.list  = mq %/% one
    n.lists   = length( ids ) %/% max.list
    by.mat    = ( max.list * n.lists )
    if( n.lists == 0 ) {
        return( paste( ids, sep=",", collapse="," ) )
    }
    else {
        mat = matrix( ids[ 1:by.mat ], nrow=n.lists, byrow=TRUE )
        r   = split( mat, row( mat ) )
        if( by.mat < length( ids ) ) {
            r = c( r, list( ids[ ( by.mat + 1 ):length( ids ) ] ) )
        }
        lapply( r, paste, sep=",", collapse="," )
    }
}

