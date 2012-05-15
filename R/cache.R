.path.for.key = function( key ) {
  paste( file.path( .xmap.internals$cache.root,
            digest( paste( .xmap.internals$species,
                   .xmap.internals$version, 
                   key, sep=':' ),
                algo='md5' ) ),
       '.xmc', sep="" )
}

.cache.store = function( key, data ) {
  path = .path.for.key( key )
  if( is.null( data ) ) {
    unlink( path )
  }
  else {
    file.handle = file( path, open="wb" )
    on.exit( close( file.handle ) )
    save( data, file=file.handle )
  }
  invisible( path )
}

.cache.retrieve = function( key ) {
  path = .path.for.key( key )
  data = NULL
  if( file.exists( path ) ) {
    load( path )
  }
  data
}

.cache.exists = function( key ) {
  path = .path.for.key( key )
  file.exists( path )
}

.set.cache.root = function( path ) {
  .xmap.internals$cache.root = path 
}

annmapClearCache = function() {
  if( is.null( .xmap.internals$initialised ) ) {
    .initialise()
  }
  file.remove( dir( .xmap.internals$cache.root, full.names=TRUE ) )
  invisible()
}

annmapToggleCaching = function() {
  if( is.null( .xmap.internals$initialised ) ) {
    .initialise()
  }
  .xmap.internals$use.cache = !.xmap.internals$use.cache
  .xmap.internals$use.cache
}