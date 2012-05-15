.get.user.string = function(v,msg) {
  if( v != "<ASK>" ) {
    r = v
  }
  else {
    cat( paste( msg, ":", sep="" ) )
    r = readLines( n=1 )
  }
  r
}

.xmc.all = function( src, params, dest=NULL ) {
  cache.id = paste( src, params$array, .xmap.internals$version, .xmap.internals$species, sep=':' )
  ret = .cache.retrieve( cache.id )
  if( is.null( ret ) ) {
    sql = .build.sql2( .xmap.queries[[ paste( src, "all", sep="." ) ]], params )
    ret = .xmc.db.call( sql, .xmap.internals$con )
    .cache.store( cache.id, ret )
  }
  ret
}

.xmc.details = function( src, params, dest=NULL ) {
  if( is.null( params ) ) {
    return( NULL )
  }
  sql = .build.sql2( .xmap.queries[[ paste( src, "details", sep="." ) ]], params )
  .xmc.db.call( sql, .xmap.internals$con )
}

.xmc.to = function( src, params, dest=NULL ) {
  if( is.null( params ) ) {
    return( NULL )
  }
  sql = .build.sql2( .xmap.queries[[ paste( src, "to", dest, sep="." ) ]], params )
  .xmc.db.call( sql, .xmap.internals$con )
}

.xmc.range = function( src, params, dest=NULL ) {
  sql = .build.sql2( .xmap.queries[[ paste( src, "range", sep="." ) ]], params )
  .xmc.db.call( sql, .xmap.internals$con )
}

.needs.array = function( action, src, dest=NULL ) {
  if( ( action == "all" || action == "details" ) && src == "array" ) {
    FALSE
  }
  else {
    sd = c( src, dest )
    any( sd == "probe" ) || any( sd == "array" ) || any( sd == "probeset" ) || any( sd == "cdnaprobeset" ) || any( sd == "exon_probeset" )
  }
}

.make.params = function( ids ) {
  if( is.null( .xmap.internals$connected ) || !.xmap.internals$connected ) {
    stop( "You need to connect to a database. -- see annmapConnect()")
  }
  if( is.null( ids ) ) {
    stop( "In .make.params, ids is NULL" )
  }
  if( is.list( ids ) ) {
    ids = unlist( ids, use.names=FALSE )
  }
  one       = max( nchar( ids ) ) + 1
  max.list  = .xmap.internals$max.query %/% one
  if( max.list == 0 ) max.list = 1
  n.lists   = length( ids )  %/% max.list
  by.mat    = ( max.list * n.lists )
  .xmap.internals$debugFn( c( "one =", one, " - max.list =", max.list, " - n.lists =", n.lists, " - by.mat =", by.mat ) )
  if( n.lists == 0 ) {
    return( paste( sapply( ids, function( b ) { dbEscapeStrings( .xmap.internals$con, b ) } ), sep=",", collapse="," ) )
  }
  else {
    .xmap.internals$debugFn( c( "Splitting into", n.lists, "queries with", max.list, "entries per query" ) )
    mat = matrix( ids[ 1:by.mat ], nrow=n.lists, byrow=TRUE )
    .xmap.internals$debugFn( c( "matrix dimensions:", dim( mat ) ) )
    r   = split( mat, row( mat ) )
    .xmap.internals$debugFn( c( "length of r is", length( r ) ) )
    if( by.mat < length( ids ) ) {
      r = c( r, list( ids[ ( by.mat + 1 ):length( ids ) ] ) )
    }
    escaped = lapply( r, function( b ) { sapply( b, function( str ) { dbEscapeStrings( .xmap.internals$con, str ) } ) } )
    return( lapply( escaped, function( row ) { paste( row, sep=',', collapse=',' ) } ) )
  }
}

.duplicate.hash = function( oldEnv ) {
  ret = new.env( hash=TRUE )
  names = ls( envir=oldEnv )
  sapply( names, function( x ) { assign( x, get( x, envir=oldEnv ), envir=ret ) } )
  ret
}

.secret.encoder = function() {
  .out = c( letters, LETTERS, 0:9, '-', '_' )
  .name = strsplit( Sys.getenv( "LOGNAME" ), split='' )[[1]]
  paste( lapply( seq_along( .name ), function( idx ) {
    .cidx = which( .out == .name[ idx ] )
    .out[ ( ( ( .cidx * 2 ) + idx ) %% length( .out ) ) + 1 ]
  } ), sep='', collapse='' )
}

.coerce = function( data, column, as.vector=TRUE ) {
  if( is.null( data ) ) {
    NULL
  }
  else if( as.vector == TRUE ) {
    data = unique( data[,column] )
  }
  else if( as.vector == FALSE ) {
    data = subset( data, select=!( colnames( data ) %in% .xmap.internals$field.mask ) )
    if( is.null( column ) && !is.null( data ) ) {
      data$IN1 = NULL
    }
    if( any( colnames( data ) == "strand" ) ) {
      colnames( data )[ colnames( data ) == "chromosome_name" ] = "space"
      data = as( data, "RangedData" )
    }
    else if( length( colnames( data ) ) == 2 && all( colnames( data ) == c( 'name', 'length' ) ) ) { # Chromosome
      colnames( data )[ colnames( data ) == "name" ] = "space"
      colnames( data )[ colnames( data ) == "length" ] = "end"
      data = cbind( data, start=1 )
      if( as.vector == FALSE ) {
        # Add in strand so we don't get warnings when converting to GRanges later
        data = cbind( data, strand='*' )
      }
      data = as( data, "RangedData" )
    }
    if( class( data ) == 'RangedData' && .usegranges() ) {
      data = as( data, 'GRanges' )
    }
  }
  data
}

.get.correct.column = function( type, data ) {
  if( class( data )[1] == 'GRanges' ) {
    .xmap.internals$debugFn( c( "Trying to select column", .xmap.types[[ type ]], "from id GRanges" ) )
    if( type == 'chromosome' ) {
      newIds = as.character( seqnames( data ) )
    }
    else {
      newIds = elementMetadata( data )[[ .xmap.types[[ type ]] ]]
    }
    if( !is.null( newIds ) ) {
      data = newIds
    }
  }
  else if( class( data ) == 'RangedData' || is.data.frame( data ) ) {
    .xmap.internals$debugFn( c( "Trying to select column", .xmap.types[[ type ]], "from id data.frame" ) )
    if( type == 'chromosome' && class( data ) == 'RangedData' ) {
      newIds = as.character( data[[ 'space' ]] )
    }
    else {
      newIds = data[[ .xmap.types[[ type ]] ]]
    }
    if( !is.null( newIds ) ) {
      data = newIds
    }
  }
  else if( is.list( data ) ) {
    .xmap.internals$debugFn( c( "Unlisting ids list of dim", dim( data ), "and length", length( data ) ) )
    newIds = unlist( data, use.names=FALSE )
    if( !is.null( newIds ) ) {
      data = newIds
    }
  }
  data
}

.process = function( action, src, params=NULL, dest=NULL ) {
  if( is.null( .xmap.internals$connected ) || !.xmap.internals$connected ) {
    stop( "You need to connect to a database. -- see annmapConnect()")
  }
  # Check we have an array set if we need one...
  if( .needs.array( action, src, dest ) && is.null( .xmap.internals$array ) ) {
    stop( "You need to set an array type. -- see arrayType()")
  }
  else if( .needs.array( action, src, dest ) ) {
    if( is.null( params ) ) {
      params = new.env( hash=TRUE )
    }
    params$array = .xmap.internals$array
  }

  # Ok, do the ids in .xmap.internals$batch.size chunks
  fname = paste( ".xmc", action, sep="." )
  .xmap.internals$debugFn( c( "Calling", fname, "[", src, ":", dest, "]" ) )

  f = .xmap.internals$procs[[ action ]]
    
  if( !is.null( params ) && !is.null( params$ids ) ) {
    params$ids = .get.correct.column( src, params$ids )
    p = .duplicate.hash( params )
    r = lapply( .xmap.internals$procs$params( params$ids ), 
      function( x ) {
        p$ids = x
        f( src, p, dest )
      }
    )
    if( !is.null( r[[ 1 ]] ) ) {
      r = do.call("rbind",r)
      rownames(r) = 1:(dim(r)[1])
      r
    }
    else {
      NULL
    }
  }
  else {
    f( src, params, dest )
    }
}

geneToSymbol = function( ids ) {
  if( is.vector( ids ) ) {
    ids = geneDetails( ids )
  }
  .attr( ids, 'symbol' )
}

allSymbols = function( as.vector=FALSE ) {
  r = sort( unique( allGenes( as.vector=F )$symbol ) )
  if( as.vector == FALSE ) {
    r = data.frame( symbol=r )
  }
  r
}

strandAsInteger = function( granges ) {
  .ret = as.character( strand( granges ) )
  .ret[ .ret == '-' ] = -1
  .ret[ .ret == '+' ] =  1
  .ret[ .ret == '*' ] = NA
  as.integer( .ret )
}

annmapRangeApply = function( x, f, filter=c( chr="space", start="start", end="end", strand="strand" ),
    coerce=c( as.character, as.numeric, as.numeric, as.numeric ), ... ) {
  if( class( x ) != 'RangedData' && class( x ) != 'GRanges' ) {
    stop( paste( 'x must be a RangedData object, not a', class( x ) ) )
  }
  if( length( coerce ) != length( filter ) ) {
    stop( paste( "Each filter column should have a corresponding coercion function: coerce:length(", length(coerce), ") filter:length(", length( filter ), ")." ) )
  }
  if( class( x ) == 'GRanges' ) {
    .str = strandAsInteger( x )
    x = as( x, 'RangedData' )
    x$strand = .str
  }
  framed.data             = as( x, "DataFrame" )
  framed.data             = framed.data[,filter,drop=FALSE]
  colnames( framed.data ) = names( filter )
  r = lapply( seq_len( dim( framed.data )[1] ), function( i ) {
    current.row = as.vector( framed.data[i,,drop=TRUE] )
                #Attempt to coerce the current row to the types defined for each parameter to 'f'
    current.row = lapply( seq_along( current.row ), function( a ) { do.call( coerce[[a]], list( current.row[[a]] ) ) } )
    params      = as.list( c( current.row, ... ) )
    do.call( f, params )
  } )
  # Strip nulls
  r = r[ !sapply( r, is.null ) ]
  clzz = unique( lapply( r, class ) )
  if( length( clzz ) == 2 && is.list( r ) ) {
    r = do.call( c, r )
    clzz = unique( lapply( r, class ) )
  }
  if( length( clzz ) == 1 ) {
    if( is.vector( r[[1]] ) ) {
      r = do.call( c, r )
    }
    else if( clzz == 'GRanges' ) {
      r = suppressWarnings( do.call( c, r ) ) # Need to supress, as we're probably looking at multipl chrs per result
    }
    else {
      r = do.call( rbind, r )
    }
  }
  r
}

geneToExonProbesetExpr = function( x, ids, probes.min=4 ) {
  if( length( ids ) == 0 ) {
    return( NULL )
  }
  if( class( x ) == 'ExpressionSet' ) {
    x = exprs( x )
  }
  pse  = geneToExonProbeset( ids, probes.min=probes.min )
  ps.r = pse[ , 'probeset' ]
  ps   = intersect( ps.r, rownames( x ) )
  r    = pse[ ps.r %in% ps, , drop=FALSE ]
  x    = as.data.frame( x[ rownames( x ) %in% ps, , drop=FALSE ] )
  x    = cbind( probeset=rownames( x ), x )
  r    = merge( x, r, by='probeset' )
  idx  = c( ( 2:length( colnames( x ) ) ), 1, ( dim( x )[ 2 ] + 1 ):length( r ) )
  r    = r[ , idx, drop=FALSE ]
  r
}

arrayType = function( name=NULL, pick.default=FALSE, silent=FALSE ) {
  # Get a list of arrays from the database
  arrs = allArrays( as.vector=FALSE )
  if( !is.null( name ) ) {
    # Check were not looking for a garbage name
    if( dim( arrs[ arrs[,1] == name, ] )[ 1 ] == 0 ) {
      stop( paste( "Array '", name, "' not found.  Call with no parameters to choose from a list", sep="" ) )
    }
    if( !silent ) {
      cat( paste( "Using array '", name, "'.\n", sep="" ) )
    }
  }
  if( is.null( name ) ) {
    if( pick.default ) {
      name = arrs[1,1]
      if( !silent ) {
        cat( paste( "Selected array '", name, "' as a default.\n", sep="" ) )
      }
    }
    else {
      choices = paste( as.character( arrs[,1] ), " ('", as.character( arrs[,2] ), "')", sep="" )
      r = menu( choices, title="Select an array type to use:" )
      if( r == 0 ) {
        if( !silent ) {
          cat( paste( "Using array '", name, "'.\n", sep="" ) )
        }
        return( invisible() )
      }
      else {
        name = as.character( arrs[r,1] )
        if( !silent ) {
          cat( paste( "Using array '", name, "'.\n", sep="" ) )
        }
      }
    }
  }
  .xmap.internals$array = name
  invisible()
}

.attr = function( o, a ) {
  if( class( o ) == 'GRanges' ) {
    o = elementMetadata( o )
  }
  o[[ a ]]
}

.usegranges = function() {
  ( is.null( annmapGetParam( 'oldstylekey' ) ) || annmapGetParam( 'oldstylekey' ) != .secret.encoder() )
}

transcriptToTranslatedprobes = function( ids ) {
    transcript.ids = .get.correct.column( 'transcript', ids )
    if( is.null( transcript.ids ) ) {
      return( NULL )
    }
    # Get all the exons for this transcript
    exons = transcriptToExon( transcript.ids )
    if( is.null( exons ) ) {
        tr.split = list()
    }
    else {
        tr.split = split( .attr( exons, 'stable_id' ), .attr( exons, 'IN1' ) )
    }
    ret = sapply( transcript.ids, function( id ) {
        exons = tr.split[[ id ]]
        exons = exonDetails( exons )
        if( !is.null( exons ) ) {
            fwd.strand = any( ( if( .usegranges() ) strandAsInteger( exons ) else exons$strand ) > 0 )
            translated.width = sum( width( exons ) )
            probes = sapply( seq_along( .attr( exons, 'stable_id' ) ), function( eidx ) {
                # Get the details for this exon
                exon = exons[ eidx, ]
                probes = probeInRange( exon )
                if( !is.null( probes ) && length( probes ) > 0 ) {
                  # Remove those that hang off the ends of the exon
                  probes = probes[ start( probes ) >= start( exon ), ]
                  probes = probes[ end( probes ) <= end( exon ), ]
                  # Check whilst we could still have both types of default class
                  if( .usegranges() ) {
                    elementMetadata( probes )$IN1 = .attr( exon, 'stable_id' )
                  }
                  else {
                    probes$IN1 = .attr( exon, 'stable_id' )
                  }
                }

                # Return the probes for this exon
                if( length( probes ) > 0 ) { probes } else { NULL }
            } )
            # Then, change these offsets so that they are relative to the start of the
            # transcript
            exon.offset = 0
            for( idx in seq_along( .attr( exons, 'stable_id' ) ) ) {
                # For the reverse strand, go through the list backwards
                eidx = if( fwd.strand ) idx else length( .attr( exons, 'stable_id' ) ) - idx + 1
                if( !is.null( probes[[ eidx ]] ) ) {
                    ranges( probes[[ eidx ]] ) = shift( ranges( probes[[ eidx ]] ),
                                                        -start( exons[ eidx, ] ) + exon.offset )
                    # If on the reverse strand, change offsets to 5' from 3'
                    if( !fwd.strand ) {
                        ranges( probes[[ eidx ]] ) = IRanges( start=translated.width - end( probes[[ eidx ]] ),
                                                              end=translated.width - start( probes[[ eidx ]] ) )
                        # Re-sort the probes by start
                        probes[[ eidx ]] = probes[[ eidx ]][ order( start( probes[[ eidx ]] ) ), ]
                    }
                  }
                exon.offset = exon.offset + width( exons[ eidx, ] )
            }
            # Remove all NULLs and combine
            if( .usegranges() ) {
              probes = probes[ !sapply( probes, is.null ) ]
              if( length( probes ) > 0 ) {
                unlist( GRangesList( probes ) )
              }
            }
            else {
              probes = do.call( 'rbind', probes[ !sapply( probes, is.null ) ] )
            }
        }
    } )
    names( ret ) = transcript.ids
    ret
}

.set.conf.dir = function() {
  tryCatch( rm( conf.dir, envir=.xmap.internals ), warning=function(a){invisible()} )
  conf.dir = Sys.getenv( "ANNMAP_HOME" )
  if( conf.dir == "" ) { 
    conf.dir = file.path( Sys.getenv( "HOME" ), ".annmap" )
    }
    cat( paste( "Using", conf.dir, "as our configuration directory.\n" ) )
  if( !file.exists( conf.dir ) ) {
    cat( paste( "Folder", conf.dir, "does not exist.  Attempting to create.\n" ) )
    dir.create( conf.dir, recursive=TRUE )
    if( !file.exists( conf.dir ) ) {
      stop( paste( "Failed to create folder '", conf.dir, "', giving up.", sep="" ) )
    }
  }
  .xmap.internals$conf.dir = conf.dir
}

annmapSetParam = function( ... ) {
  .params = list( ... )
  for( .name in names( .params ) ) {
    .xmap.internals[[ .name ]] = .params[ .name ][[1]]
  }
  if( !is.null( .xmap.internals$debug ) && ( .xmap.internals$debug ) ) {
    .xmap.internals$debugFn = .debug.full
  }
  else {
    .xmap.internals$debugFn = .debug.none
  }
}

annmapGetParam = function( key ) {
  .xmap.internals[[ key ]]
}

.debug.none = function( message ) {
}

.debug.full = function( message ) {
  cat( format( Sys.time(), "%a %b %d %X %Y" ), "::", message, "\n" )
}

.initialise = function( use.cache=TRUE ) {
  .set.conf.dir()
  .xmap.internals$debug = FALSE
  .xmap.internals$debugFn = .debug.none
  .xmap.internals$max.query = 10000
  .xmap.internals$connected = FALSE
  .xmap.internals$field.mask = c( 'array_id', 'chromosome_id', 'hit_id', 'protein_id', 'gene_id', 'transcript_id', 'exon_id', 'probe_id', 'probeset_id', 'synonym_id', 'external_db_id' )
  
  # Put all our main calls in an internal hash
  # We should then be able to call annmapSetParam with a new hash
  # to override these (eg for using a webservice rather than a db)
  .procs = .make.hash()
  .procs$all = .xmc.all
  .procs$details = .xmc.details
  .procs$to = .xmc.to
  .procs$range = .xmc.range
  .procs$params = .make.params
  .procs$connect = .xmc.connect
  .procs$disconnect = .xmc.disconnect
  .xmap.internals$procs = .procs
  .xmap.internals$initialised = TRUE

  # Set the cache root
  path = file.path( .xmap.internals$conf.dir, "cache" )
  if( !file.exists( path ) ) {
    r = dir.create( path )
    if( !r ) {
      warning( paste( "Could not create or locate cache folder '", path, "', running with caching disabled", sep="" ) )
      path = NULL
      use.cache = FALSE
    }
    else {
      use.cache = TRUE
    }
  }
  .xmap.internals$use.cache = use.cache
  .set.cache.root( path )
}

generalisedNameToNCBI = function( name, ... ) {
  if( length( grep( 'chr', name ) ) > 0 ) { name }
  else if( name == 'MT' )                 { 'chrM' }
  else                                    { paste( 'chr', name, sep='' ) }
}

generalisedNameToEnsembl = function( name, ... ) {
  if( length( grep( 'chr', name ) ) == 0 ) { name }
  else if( name == 'chrM' )                { 'MT' }
  else                                     { substring( name, 4 ) }
}

seqnameMapping = function( x, mappingFunction, ... ) {
  seqlevels( x ) = unlist( lapply( seqlevels( x ), function( a ) { mappingFunction( a, ... ) } ) )
  x
}

seqnamesToNCBI = function( x ) {
  seqnameMapping( x, generalisedNameToNCBI )
}

seqnamesToEnsembl = function( x ) {
  seqnameMapping( x, generalisedNameToEnsembl )
}

annmapEnv = function() {
  if( is.null( .xmap.internals$connected ) || !.xmap.internals$connected ) {
    stop( "You need to connect to a database. -- see annmapConnect()")
  }
  .xmc.db.call( "CALL env()", .xmap.internals$con )
}
