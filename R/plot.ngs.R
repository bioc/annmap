ngsTracePlotter = function( rle.data, start, end, ylim, trace.label.properties=list(),
                            smoothing.function=function( rle, ... ) { runmean( rle, k=1001, endrule='constant' ) }, 
                            trace.clip='inherit', trace.draw.scale=FALSE, trace.bor='transparent', trace.pad=c(0,0), ... ) {
  .y = 0
  local.draw = function( start, rle, col, alpha.mod=1.0, mult=1, local.draw.outline=T, local.draw.fill=T, ... ) {
    .len = runLength( rle )
    .len[ 1 ] = .len[ 1 ] - start
    .last.val = start
    .xpts = c( start )
    for( v in seq_along( .len ) ) {
      .xpts = c( .xpts, c( .last.val, .len[ v ] + .last.val ) )
      .last.val = .len[ v ] + .last.val
    }
    .xpts = c( .xpts, .last.val, end )
    .val = c( .y, unlist( lapply( runValue( rle ), function( v ) { .val = v ; c( .val, .val ) } ) ), .y, .y )
    grid.move.to( start, 0, default.units='native' )
    grid.line.to( end, 0, gp=gpar( col='black', alpha=0.1 ), default.units='native' )
    if( local.draw.fill ) {
      grid.polygon( x=.xpts, y=.val * mult, gp=gpar( fill=col, col=NA, alpha=0.5*alpha.mod ), default.units='native' )
    }
    if( local.draw.outline ) {
      grid.polygon( x=.xpts, y=.val * mult, gp=gpar( fill=NA, col=col, alpha=alpha.mod ), default.units='native' )
    }
  }
  
  lheights = unit( c(    top=trace.pad[1], main=1,      bottom=trace.pad[2] ),
                   c(    top='lines',      main='null', bottom='lines' ),
                   list( top=NULL,         main=NULL,   bottom=NULL ) )
  lwidths  = unit( 1, 'null', NULL )

  my.layout = grid.layout( nrow = 3, ncol = 1, heights = lheights, widths = lwidths )
  pushViewport( viewport( layout=my.layout ) )

  pushViewport( viewport( layout.pos.col=1, layout.pos.row=2 ) )
    pushViewport( viewport( layout=grid.layout( 1, 1 ), xscale=c( start, end ), yscale=ylim, clip=trace.clip ) )
      if( !is.list( rle.data$rle ) && length( rle.data$rle ) > 0 ) {
        if( !is.null( smoothing.function ) ) {
          local.draw( start, rle.data$rle, 'black', alpha.mod=0.3, ... )
          params = as.list( c( rle=rle.data$rle, rle.data[ names( rle.data ) != 'rle' ] ) )
          .smooth = do.call( smoothing.function, params )
          local.draw( start, .smooth, rle.data$col, ... )
          rle.data$rle = .smooth
        }
        else {
          local.draw( start, rle.data$rle, rle.data$col, ... )
        }
      }
      else if( length( rle.data$rle ) == 2 ) {
        if( !is.null( rle.data$rle$'+' ) && length( rle.data$rle$'+' ) > 0 ) {
          if( !is.null( smoothing.function ) ) {
            local.draw( start, rle.data$rle$'+', 'black', alpha.mod=0.3, ... )
            params = as.list( c( rle=rle.data$rle$'+', strand=1, rle.data[ names( rle.data ) != 'rle' ] ) )
            .smooth.fwd = do.call( smoothing.function, params )
            local.draw( start, .smooth.fwd, rle.data$col, ... )
            rle.data$rle$'+' = .smooth.fwd
          }
          else {
            local.draw( start, rle.data$rle$'+', rle.data$col, ... )
          }
        }
        if( !is.null( rle.data$rle$'-' ) && length( rle.data$rle$'-' ) > 0 ) {
          if( !is.null( smoothing.function ) ) {
            local.draw( start, rle.data$rle$'-', 'black', alpha.mod=0.3, mult=-1, ... )
            params = as.list( c( rle=rle.data$rle$'-', strand=-1, rle.data[ names( rle.data ) != 'rle' ] ) )
            .smooth.rev = do.call( smoothing.function, params )
            local.draw( start, .smooth.rev, rle.data$col, mult=-1, ... )
            rle.data$rle$'-' = .smooth.rev
          }
          else {
            local.draw( start, rle.data$rle$'-', rle.data$col, mult=-1, ... )
          }
        }
      }
      else {
        grid.polygon( x=c( start, end ), y=c( .y, .y ), gp=gpar( fill=NA, col=rle.data$col ), default.units='native' )
      }
      .text.defaults = list( rle.data$name, x=unit( 0.1, 'npc' ), y=unit( 0.9, 'npc' ), just=c( 'left', 'top' ), gp=gpar( cex=0.7, col=rle.data$col, alpha=0.8 ) )
      if( class( trace.label.properties ) == 'list' ) {
        .text.defaults = modifyList( .text.defaults, trace.label.properties )
        grid.text( rle.data$name, x=.text.defaults$x, y=.text.defaults$y, just=.text.defaults$just, gp=.text.defaults$gp )
      }

  popViewport( 2 )
  pushViewport( viewport( layout.pos.col=1, layout.pos.row=2, xscale=c( start, end ), yscale=ylim ) )
    if( ( is.vector( trace.draw.scale ) && ( 'x' %in% trace.draw.scale ) ) ||
        ( is.list( trace.draw.scale ) && !is.null( trace.draw.scale$x ) ) ||
        ( is.logical( trace.draw.scale ) && trace.draw.scale ) ) {
      grid.xaxis( gp=gpar( cex=0.6 ), main=if( is.list( trace.draw.scale ) ) trace.draw.scale$x else TRUE )
    }
    if( ( is.vector( trace.draw.scale ) && 'y' %in% trace.draw.scale ) ||
        ( is.list( trace.draw.scale ) && !is.null( trace.draw.scale$y ) ) ||
        ( is.logical( trace.draw.scale ) && trace.draw.scale ) ) {
      grid.yaxis( gp=gpar( cex=0.6 ), main=if( is.list( trace.draw.scale ) ) trace.draw.scale$y else TRUE )
    }
  popViewport( 2 )
  rle.data
}

ngsTraceScale = function( vector.of.xbams.and.ybams ) {
  c( min( unlist( lapply( vector.of.xbams.and.ybams, function( d ) {
      if( length( d$rle ) == 0 ) {
        0
      }
      else if( !is.list( d$rle ) ) { # Just a single strand
        min( runValue( d$rle ) )
      }
      else { # Two strands '+' and '-'
        -max( runValue( d$rle$'-' ) )
      }
     } ) ) ),
   max( unlist( lapply( vector.of.xbams.and.ybams, function( d ) {
      if( length( d$rle ) == 0 ) {
        0
      }
      else if( !is.list( d$rle ) ) { # Just a single strand
        max( runValue( d$rle ) )
      }
      else { # Two strands '+' and '-'
        max( runValue( d$rle$'+' ) )
      }
   } ) ) ) )
}

ngsTraceLabel = function( rle.data ) {
  grid.text( rle.data$name, x=unit( 0.1, 'npc' ), just=c( 'left', 'center' ), gp=gpar( cex=0.8 ) )
}

convertBamToRle = function( bam.file.name, chr, start, end, chr.name.mapping=function( name ) { name } ) {
  chr = chr.name.mapping( chr )
  if( missing( start ) ) {
    start = 1
  }
  if( missing( end ) ) {
    end = chromosomeDetails( chr )$length
  }
  # Set the chromosome and range of interest (tried using a GRange to specify strand as well, but it doesnt work
  which  = RangedData( space=chr, ranges=IRanges( start=as.integer(start), end=as.integer(end) ) )
  # set parameters as described above for reading bam file
  param  = ScanBamParam( flag=scanBamFlag( isUnmappedQuery=F ), what=c( 'rname', 'strand', 'pos', 'qwidth' ), which=which )
  # read data from current bam file (for a given project, this will have to be applied over each timepoint/sub-project)
  BAM    = do.call( 'rbind', lapply( bam.file.name, function( fn ) { as.data.frame( scanBam( fn, param=param ) ) } ) )
  colnames(BAM) = c( 'space', 'strand', 'start', 'width' )
  BAM = BAM[ with( BAM, order( start ) ), ]
  sapply( c( '+', '-' ), function( str ) {
    d = BAM[ as.character( BAM[,2] ) == str, ]
    coverage( as( cbind( d, end=( d$start + d$width - 1 ) ), 'RangedData' ) )[[ chr ]]
  }, simplify=F )
}

generateBridgeData = function( xrange, bamFiles, colours=NULL, names=NULL ) {
  if( is.null( names ) ) names = paste( "Track", seq_along( bamFiles ) )
  if( is.null( colours ) ) colours = rainbow( length( bamFiles ), v=0.5, s=0.5 )
  apply( cbind( bamFiles, colours, names ), 1, function( r ) {
    list( name=r$names, col=r$colours, rle=convertBamToRle( r$bamFiles, as.character( seqnames( xrange ) ), start( xrange ), end( xrange ) ) )
  } )
}

ngsBridgePlot = function( xrange, data=list(), 
                          main=NULL,
                          sub=NULL,
                          highlights=NULL,
                          trace.plotter=ngsTracePlotter,
                          genome.layout.weight=4,
                          trace.scale=ngsTraceScale,
                          trace.draw.scale=NULL,
                          trace.match.strand=TRUE,
                          probe.plot=NULL,
                          exon.depth.plot=genomicExonDepthPlot, 
                          .genes=NULL,
                          .exons=NULL,
                          ... ) {
  .lh = c( main = if( !is.null( main ) ) 2 else 0,
           gen  = genome.layout.weight,
           sapply( seq_along( data ), function( f ) { r = 1 ; names( r ) = paste( 'dat', f, sep='' ) ; r } ),
           pad  = 1,
           sub  = if( !is.null( sub ) ) 2 else 0 )
  .lt = c( main = if( !is.null( main ) ) 'strheight' else 'null',
           gen  = 'null',
           sapply( seq_along( data ), function( f ) { r = 'null' ; names( r ) = paste( 'dat', f, sep='' ) ; r } ),
           pad  = 'lines',
           sub  = if( !is.null( sub ) ) 'strheight' else 'null' )
  .ld = c( list( main = if( is.null( main ) ) NULL else 'main',
                 gen  = NULL ),
           sapply( seq_along( data ), function( f ) { a = list( a=NULL ) ; names( a ) = paste( 'dat', f, sep='' ) ; a } ),
           list( pad = NULL,
                 sub = if( is.null( sub ) ) NULL else 'sub' ) )
  lheights = unit( .lh, .lt, .ld )
  lwidths  = unit( c( lpad = 2, gen = 1, rpad = 2 ),
                   c( lpad = 'lines', gen = 'null', rpad = 'lines' ),
                   list( lpad = NULL, gen = NULL, rpad = NULL ) )
  my.layout = grid.layout( nrow = 4 + length( data ), ncol = 3, heights = lheights, widths = lwidths )

  if( is.null( .genes ) ) {
    .genes = .fetch.genes.for.range( xrange, ... )
  }
  else {
    .genes = .get.correct.column( 'gene', .genes )
    .genes = geneDetails( .genes, as.data.frame=T )
  }
  .all.exons = geneToExon( if( !is.null( .genes ) ) .genes$stable_id else NULL )
  if( !is.null( .exons ) ) {
    # Filter .all.exons to just include those in .exons
    .exon.names = .get.correct.column( 'exon', .exons )
    .all.exons = .all.exons[ .attr( .all.exons, 'stable_id' ) %in% .exon.names, ]
  }

  .exons = if( is.null( .all.exons ) ) list() else lapply( split( .attr( .all.exons, 'stable_id' ), factor( .attr( .all.exons, 'IN1' ) ) ), function( .name ) {
    .rows = reduce( ranges( .all.exons[ .attr( .all.exons, 'stable_id' ) %in% .name, ] ) )
  } )

  .skip.probes = identical( probe.plot, genomicProbePlot ) && ( length( allArrays( as.vector=TRUE ) ) == 0 )

  pushViewport( viewport( layout=my.layout ) )
    if( !is.null( main ) ) {
      pushViewport( viewport( layout.pos.row=1, layout.pos.col=1:3 ) )
        grid.text( main )
      popViewport()
    }
    if( !is.null( sub ) ) {
      pushViewport( viewport( layout.pos.row=4 + length( data ), layout.pos.col=1:3 ) )
        grid.text( sub, gp=gpar( cex=0.8 ) )
      popViewport()
    }
    pushViewport( viewport( layout.pos.row=2:(2+length(data)), layout.pos.col=2 ) )
      .strand = if( class( xrange ) == 'GRanges' ) {
        if( as.character( strand( xrange ) ) == '*' ) {
          NULL
        }
        else {
          strandAsInteger( xrange )
        }
      }
      else {
        xrange$strand
      }
      .filtered.gene.names = if( is.null( .strand ) ) .genes$stable_id else .genes[ .genes$strand==.strand, ]$stable_id
      if( !is.null( exon.depth.plot ) ) {
        exon.depth.plot( .exons[ names( .exons ) %in% .filtered.gene.names ], start( xrange ), end( xrange ), ... )
      }
      if( !.skip.probes && !is.null( probe.plot ) ) {
        .chr = if( class( xrange ) == 'GRanges' ) {
          as.character( seqnames( xrange ) )
        }
        else {
          as.character( space( xrange ) )
        }
        .probes = if( is.null( .strand ) ) {
          rbind( probeInRange( .chr, start( xrange ), end( xrange ), 1, as.vector='data.frame' ),
                 probeInRange( .chr, start( xrange ), end( xrange ), -1, as.vector='data.frame' ) )
        }
        else {
          probeInRange( xrange, as.vector='data.frame' )
        }
        .probes = .probes[ .probes$probe_hit_count == 1, ]
        probe.plot( .probes, start( xrange ), end( xrange ), ... )
      }
    popViewport()

    pushViewport( viewport( layout.pos.row=2, layout.pos.col=2 ) )
      genomicPlot( xrange, highlights=highlights, exon.depth.plot=NULL, .genes=.genes, .exons=.exons, ... )
    popViewport()

    if( length( data ) > 0 ) {
      dataStrandFn = function( element ) {
        if( is.list( element$rle ) ) {
          # Single strand data... Just return it as is
          if( trace.match.strand == TRUE ) {
            strand = as.character( strand( xrange ) )
            if( strand != '*' ) {
              element$rle = element$rle[[ strand ]]
            }
          }
          else if( trace.match.strand == '+' || trace.match.strand == '-' ) {
            element$rle = element$rle[[ trace.match.strand ]]
          }
        }
        element
      }
      data = lapply( data, dataStrandFn )
      local.scale = if( is.vector( trace.scale ) ) trace.scale else trace.scale( data )
      lapply( seq_along( data ), function( rleidx ) {
        pushViewport( viewport( layout.pos.row=2 + rleidx, layout.pos.col=2, xscale=c( start( xrange ), end( xrange ) ), yscale=local.scale ) )
          if( is.null( trace.draw.scale ) )
            trace.draw.scale = if( rleidx==1 ) 'y' else FALSE
          trace.plotter( data[[ rleidx ]], start( xrange ), end( xrange ), local.scale, trace.draw.scale=trace.draw.scale, ... )
        popViewport()
      } )
    }
  popViewport()
}
