.fetch.genes.for.range = function( pos, drop.partial.genes=FALSE, ... ) {
  .genes = geneInRange( pos, as.vector='data.frame' )
  if( !is.null( .genes ) ) {
    .genes = .genes[with(.genes, order( start )),]
    if( drop.partial.genes ) {
      .genes = .drop.partial.genes( .genes, start( pos ), end( pos ) )
    }
  }
  .genes
}

.drop.partial.genes = function( genes, start, end ) {
  for( i in seq_along( genes[ genes$start < start, ]$stable_id ) ) {
    warning( paste( 'Dropping partial gene', genes[ genes$start < start, ][ i, ]$name, '(falls off start)' ) )
  }
  genes = genes[ genes$start >= start, ]
  for( i in seq_along( genes[ genes$end > end, ]$stable_id ) ) {
    warning( paste( 'Dropping partial gene', genes[ genes$end > end, ][ i, ]$name, '(falls off end)' ) )
  }
  genes[ genes$end <= end, ]
}

.layout.genes = function( .genes, start, end, gene.area.height=NULL, gene.layout.padding=100, ... ) {
  # Add a y column for the layout
  .genes = cbind( .genes, y=rep( 1, dim(.genes)[1] ) )

  # layout the strands separately, then join them back togther
  .strands = c( 1, -1 )
  for( .s in seq_along( .strands ) ) {
    .max.x.in.layer = c( 1 )
    for( i in seq_along( .genes[ .genes$strand==.strands[.s], ]$stable_id ) ) {
      .row = .genes[ .genes$strand==.strands[.s], ][ i, ]
      while( T ) {
        if( .row$start < .max.x.in.layer[ .row$y ] ) {
          if( .row$y + 1 > length( .max.x.in.layer ) ) .max.x.in.layer = c( .max.x.in.layer, 1 )
          .row$y = .row$y + 1
        }
        else {
          break
        }
      }
      .max.x.in.layer[ .row$y ] = max( .row$end, .row$start + ( ( end - start ) * convertWidth( unit( strwidth( .row$name, units='inches' ), 'inches' ), 'npc', valueOnly=T ) ) ) + gene.layout.padding
      .genes[ .genes$strand==.strands[.s], ][ i, ] = .row
    }
  }
  .reverse.height = if( length( .genes[ .genes$strand==-1, ]$y ) > 0 ) max( .genes[ .genes$strand==-1, ]$y ) else 0
  .forward.height = if( length( .genes[ .genes$strand== 1, ]$y ) > 0 ) max( .genes[ .genes$strand== 1, ]$y ) else 0

  # reverse genes need flipping upside down
  for( i in seq_along( .genes[ .genes$strand==-1, ]$stable_id ) ) {
    .genes[ .genes$strand==-1, ][ i, ]$y = .reverse.height - .genes[ .genes$strand==-1, ][ i, ]$y + 1
  }

  if( is.null( gene.area.height ) ) {
    .reverse.height = max( .reverse.height, .forward.height )
    .forward.height = .reverse.height
  }
  else if( is.numeric( gene.area.height ) ) {
    .reverse.height = gene.area.height
    .forward.height = gene.area.height
    for( i in seq_along( .genes$stable_id ) ) {
      if( .genes[ i, ]$y > gene.area.height ) {
        .genes[ i, ]$y = 0
        warning( paste( 'Gene', .genes[ i, ]$name, 'dropped, as it falls outside of gene.area.height' ) )
      }
    }
    .genes = .genes[ .genes$y > 0, ]
  }
  list( genes=.genes, fwd.height=.forward.height, rev.height=.reverse.height )
}

.draw.background.arrow = function( x, y, alpha ) {
  grid.polygon( x=x, y=y, id=rep( 1, length( x ) ), default.units='npc', gp=gpar( fill='black', alpha=alpha ) )
}

.draw.genes = function( .genes, .exons, start, end, alpha ) {
  for( i in seq_along( .genes$stable_id ) ) {
    .row = .genes[ i, ]
    .col = as.character( .row$col )
    .bor = as.character( .row$bor )
    .ex = .exons[[ as.character( .row$stable_id ) ]]
    if( !is.null( .ex ) ) {
      grid.text( .row$name, .row$start, 0,
                 just=c('left', 'bottom'), default.units='native',
                 gp=gpar( adj=c(0,1), col=.bor, fontsize=convertHeight( unit( 0.4, 'npc' ), 'points' ) , alpha=alpha ) )
      .ex = .ex[ end( .ex ) >= start, ]
      .ex = .ex[ start( .ex ) <= end, ]
      len = if( .usegranges() ) length( .ex ) else length( .ex[[ 1 ]] )
      if( len > 0 ) {
        grid.segments( max( start, as.numeric( unlist( start( .ex ) ) ) ), 0.6,
                       min(   end, as.numeric( unlist(   end( .ex ) ) ) ), 0.6,
                       default.units='native', gp=gpar( lty=3, alpha=alpha ) )
        grid.rect( unlist( lapply( as.integer( unlist( start( .ex ) ) ), function( v ) { max( v, start ) } ) ), 0.4,
                   unlist( lapply( as.integer( unlist(   end( .ex ) ) ), function( v ) {   min( v, end ) } ) ) - as.integer( unlist( start( .ex ) ) ), 0.4,
                   just=c('left','bottom'), default.units='native', gp=gpar( fill=.col, alpha=alpha ) )
      }
    }
  }
}

.draw.strand = function( .genes, .exons, start, end, strand, gene.area.height, alpha, ... ) {
  .draw.background.arrow( if( strand > 0 ) c( 0, 0.9, 1,   0.9, 0 ) else c( 0,   0.1, 1, 1, 0.1 ),
                          if( strand > 0 ) c( 0, 0,   0.5, 1,   1 ) else c( 0.5, 0,   0, 1, 1 ), alpha * 0.15 )
  # Then, push a new layout into this cell, and draw the genes for each
  if( !is.null( .genes ) ) {
    lheights = unit( rep( 1, gene.area.height + 2 ), rep( 'null', gene.area.height + 2 ), rep( list( NULL ), gene.area.height + 2 ) )
    lwidths  = unit( c( 1 ), c( 'null' ), NULL )
    my.layout = grid.layout( nrow=gene.area.height + 2, ncol=1, heights=lheights, widths=lwidths )
    pushViewport( viewport( layout=my.layout ) )
      lapply( seq( 2, gene.area.height + 1 ), function( y ) {
        pushViewport( viewport( layout.pos.col=1, layout.pos.row=y, xscale=c( start, end ), yscale=c( 0, 1 ) ) )
          .draw.genes( .genes[ .genes$y==y - 1, ], .exons, start, end, alpha )
        popViewport()
      } )
    popViewport()  
  }
}

# Does this data.frame have anything in it?
.has.data = function( df ) {
  ( !is.null( df ) ) && ( dim( df )[ 1 ] > 0 )
}

genomicExonDepthPlot = function( .exons, start, end, exon.depth.alpha=0.1, exon.depth.col='black', ... ) {
  .to.dataframe = function( f ) {
    data.frame( start=as.integer(unlist(start(f))), end=as.integer(unlist(end(f))) )
  }
  .exons = do.call( 'rbind', lapply( .exons, .to.dataframe ) )

  pushViewport( viewport( layout=grid.layout( 1, 1 ), xscale=c( start, end ), yscale=c( 0, 1 ) ) )
    if( !is.null( .exons ) ) {
      # Drop all exons outside of region
      .exons = .exons[ .exons$end >= start, ]
      .exons = .exons[ .exons$start <= end, ]
      starts = unlist( lapply( .exons$start, function( v ) { max( v, start ) } ) )
      ends   = unlist( lapply( .exons$end, function( v ) {   min( v, end ) } ) ) - .exons$start
      if( length( starts ) > 0 ) {
        grid.rect( starts, 0,
                   ends, 1,
                   just=c('left','bottom'),
                   default.units='native',
                   gp=gpar( col=NA, fill=exon.depth.col, alpha=exon.depth.alpha ) )
      }
    }
  popViewport()
}

genomicProbePlot = function( probes, start, end, probe.col='green', probe.alpha=0.3, ... ) {
  pushViewport( viewport( layout=grid.layout( 1, 1 ), xscale=c( start, end ), yscale=c( 0, 1 ) ) )
    if( !is.null( probes ) ) {
      probes = probes[ probes$end >= start, ]
      probes = probes[ probes$start <= end, ]
      starts = unlist( lapply( probes$start, function( v ) { max( v, start ) } ) )
      ends   = unlist( lapply( probes$end, function( v ) { min( v, end ) } ) ) - probes$start
      if( length( starts ) > 0 ) {
        grid.rect( starts, 0,
                   ends, 1,
                   just=c('left', 'bottom'),
                   default.units='native',
                   gp=gpar( fill=probe.col, col=NA, alpha=probe.alpha ) )
      }
    }
  popViewport()
}

genomicPlot = function( xrange,               # An IRanges object representing the region of interest (with a strand if reqd)
              gene.area.height=NULL,           # NULL=Both strands to max height of either of them, integer=Both strands limited to this height, NA=Both strands limited to their implied height 
              gene.layout.padding=100,         # How much space needs to be between each gene in a layer
              highlights=NULL,                 # Some highlighted regions (as a data.frame with start, end, strand and name columns)
              draw.opposite.strand=FALSE,      # Do we draw a washed out representation of the other strand. Only applies if !is.null(xrange$strand)
              exon.depth.plot=genomicExonDepthPlot, # Draw the exondepth per strand? Function if so, NULL if not
              padding.lines=1,                 # how much padding above and below the plot (in grid lines)
              .genes=NULL,                     # Pass in pre-loaded .genes and .exons if you want (then we skip loading them in here)
              .exons=NULL,
              invert.strands=FALSE,            # Forward strand on the bottom?
              draw.scale=TRUE,                 # Do we draw a scale?
              ... ) {                          # Other params passed to genomic.exon.depth.plot, .draw.strand and .draw.genes
  if( is.null( xrange ) ) {
    stop( 'Parameter \'xrange\' cannot be NULL' )
  }
  if( is.null( .genes ) ) {
    .genes = .fetch.genes.for.range( xrange, ... )
  }
  else {
    .genes = .get.correct.column( 'gene', .genes )
    .genes = geneDetails( .genes, as.data.frame=T )
  }
  .strand = if( class( xrange ) == 'GRanges' ) strandAsInteger( xrange ) else { if( xrange$strand == 0 ) NULL else xrange$strand }
  if( !is.null( .genes ) &&
      ( ( class( xrange ) == 'GRanges' && !is.na( .strand ) ) ||
        ( class( xrange ) != 'GRanges' && !is.null( .strand ) ) ) && 
      !draw.opposite.strand ) {
    # remove the opposite strand
    .genes = .genes[ .genes$strand == .strand, ]
  }
  if( is.null( .exons ) ) {
    .all.exons = geneToExon( if( is.null( .genes ) ) NULL else .genes$stable_id )
    .exons = if( is.null( .all.exons ) ) {
      list()
    }
    else {
      sid = .attr( .all.exons, 'stable_id' )
      in1 = .attr( .all.exons, 'IN1' )
      lapply( split( sid, factor( in1 ) ), function( .name ) {
        .rows = reduce( ranges( .all.exons[ sid %in% .name, ] ) )
      } )
    }
  }
  if( !is.null( .genes ) ) {
    .genes = do.call( 'rbind', lapply( split( .genes$stable_id, factor( .genes$stable_id ) ), function( .name ) {
      .row = .genes[ .genes$stable_id==.name, ]
      data.frame( stable_id=.name, symbol=.row$symbol, start=.row$start, end=.row$end, strand=.row$strand,
                  name=paste( if( !is.null( .row$symbol ) ) .row$symbol else .row$stable_id, if( .row$strand > 0 ) '(+)' else '(-)' ),
                  col=rgb(0,0,0), bor=rgb(0,0,0) )
    } ) )
    .genes = .genes[with(.genes, order( start )),]
  }

  # If we have dummy highlight regions...
  if( .has.data( highlights ) ) {
    .required.fields = c( 'name', 'start', 'end', 'strand' )
    if( length( unique( colnames( highlights )[ colnames( highlights ) %in% .required.fields ] ) ) != length( .required.fields ) ) {
      warning( paste( c( 
        'Missing required column names in highlights data.frame',
        paste( '  REQUIRED:', paste( .required.fields, collapse=', ' ) ),
        paste( '  RECIEVED:', paste( colnames( highlights ), collapse=', ' ) ),
        'Highlights will be ignored' ), collapse='\n' ) )
    }
    else {
      # Filter out just the columns we are interested in...
      .columns = c( 'name', 'start', 'end', 'strand' )
      if( 'col' %in% colnames( highlights ) ) .columns = c( .columns, 'col' )
      if( 'bor' %in% colnames( highlights ) ) .columns = c( .columns, 'bor' )
      highlights = highlights[ , .columns ]

      # Hack in defaults...
      highlights = cbind( highlights, stable_id=paste( 'a', 1:dim( highlights )[ 1 ], sep='' ) )
      highlights = cbind( highlights, symbol=rep( '', dim( highlights )[ 1 ] ) )
      if( is.null( highlights$col ) ) {
        highlights = cbind( highlights, col=rep( rgb( 0.9, 0.7, 0.45 ), dim( highlights )[ 1 ] ) )
      }
      if( is.null( highlights$bor ) ) {
        highlights = cbind( highlights, bor=rep( rgb( 0.5, 0, 0 ), dim( highlights )[ 1 ] ) )
      }

      # Attach and re-sort
      .genes = rbind( .genes, highlights )
      .genes = .genes[with(.genes, order( start )),]

      # Then generate an exon the full width for each gene
      .new.exons = lapply( seq_along( highlights$stable_id ), function( idx ) {
        .row = highlights[ idx, ]
        IRanges( start=.row$start, end=.row$end )
      } )
      names(.new.exons) = highlights$stable_id

      .exons = c( .exons, .new.exons )
    }
  }

  if( !is.null( .genes ) ) {
    .genes = .layout.genes( .genes, start(xrange), end(xrange), gene.area.height=gene.area.height, ... )
  }

  # Build our grid viewport and push it in
  .lh = if( !is.null( .genes ) ) {
    c( top=padding.lines,
       fwd=if( is.null( .strand ) || is.na( .strand ) || ( as.numeric( .strand ) == 1 ) || draw.opposite.strand ) { if( invert.strands ) .genes$rev.height + 2 else .genes$fwd.height + 2 } else 0,
       scagap=if( draw.scale ) 1.5 else 0,
       sca=if( draw.scale ) 1 else 0,
       rev=if( is.null( .strand ) || is.na( .strand ) || ( as.numeric( .strand ) == -1 ) || draw.opposite.strand ) { if( invert.strands ) .genes$fwd.height + 2 else .genes$rev.height + 2 } else 0,
       bot=padding.lines )
  }
  else {
    c( top=padding.lines,
       fwd=if( is.null( .strand ) || is.na( .strand ) || ( as.numeric( .strand ) == 1  ) || draw.opposite.strand ) 1 else 0,
       scagap=if( draw.scale ) 1.5 else 0,
       sca=if( draw.scale ) 1 else 0,
       rev=if( is.null( .strand ) || is.na( .strand ) || ( as.numeric( .strand ) == -1 ) || draw.opposite.strand ) 1 else 0,
       bot=padding.lines )
  }
  .lt = c( top=if( padding.lines > 0 ) 'lines' else 'null',
           fwd='null',
           scagap=if( draw.scale ) 'lines' else 'null',
           sca=if( draw.scale ) 'lines' else 'null',
           rev='null',
           bot=if( padding.lines > 0 ) 'lines' else 'null' )
  .ld = list( top=NULL,
              fwd=NULL,
              scagap=NULL,
              sca=NULL,
              rev=NULL,
              bot=NULL )
  lheights = unit( .lh, .lt, .ld )
  lwidths  = unit( c( w=1 ), c( w='null' ), list( w=NULL ) )
  my.layout = grid.layout( nrow=6, ncol=1, heights=lheights, widths=lwidths )
  pushViewport( viewport( layout=my.layout ) )

    # draw the scale in the central viewport
    if( draw.scale ) {
      pushViewport( viewport( layout.pos.col=1, layout.pos.row=4, xscale=c( start(xrange), end(xrange) ), yscale=c( 0, 1 ) ) )
        grid.xaxis( main=FALSE, gp=gpar( cex=0.6 ) )
      popViewport()
    }

    # draw the forward strand if required
    if( is.null( .strand ) || is.na( .strand ) || ( as.numeric( .strand ) == 1 ) || draw.opposite.strand ) {
      alp = if( !is.null( .strand ) && !is.na( .strand ) && ( as.numeric( .strand ) == -1 ) ) 0.3 else 1.0
      pushViewport( viewport( layout.pos.col=1, layout.pos.row=if( invert.strands ) 5 else 2 ) )
        .plot.genes = .genes$genes[ .genes$genes$strand==1, ]
        if( !is.null( exon.depth.plot ) ) {
          exon.depth.plot( .exons=.exons[ names( .exons ) %in% .plot.genes$stable_id ], start=start(xrange), end=end(xrange), ... )
        }
        .draw.strand( .plot.genes, .exons, start(xrange), end(xrange), 1, .genes$fwd.height, alp )
      popViewport()
    }

    # draw the reverse strand if required
    if( is.null( .strand ) || is.na( .strand ) || ( as.numeric( .strand ) == -1 ) || draw.opposite.strand ) {
      alp = if( !is.null( .strand ) && !is.na( .strand ) && ( as.numeric( .strand ) != -1 ) ) 0.3 else 1.0
      pushViewport( viewport( layout.pos.col=1, layout.pos.row=if( invert.strands ) 2 else 5 ) )
        .plot.genes = .genes$genes[ .genes$genes$strand==-1, ]
        if( !is.null( exon.depth.plot ) ) {
          exon.depth.plot( .exons=.exons[ names( .exons ) %in% .plot.genes$stable_id ], start=start(xrange), end=end(xrange), ... )
        }
        .draw.strand( .plot.genes, .exons, start(xrange), end(xrange), -1, .genes$rev.height, alp )
      popViewport()
    }
  popViewport()
}
