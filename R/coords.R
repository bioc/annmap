# -------------------------------------------------------------------------------
# Transcript coords to genome

.single.transcript.coords.to.genome = function( transcript.id, position, exons, check.bounds, truncate ) {
  strand     = as.numeric( unique( exons$strand ) )
  space      = as.character( unique( exons$chromosome_name ) )

  if( dim( exons )[1] == 0 ) {
    return( data.frame( start=NA, end=NA, chromosome_name=NA, strand=NA, IN1=transcript.id, stringsAsFactors=FALSE ) )
  }
  if( position < 1 && check.bounds ) {
    warning( paste( 'Fell off the start of transcript', transcript.id, 'looking for position', position ) )
    return( data.frame( start=NA, end=NA, chromosome_name=space, strand=strand, IN1=transcript.id, stringsAsFactors=FALSE ) )
  }

  exons      = exons[ order( exons$start, decreasing=strand < 0 ), ]
  widths     = exons$end - exons$start + 1 # exons with the same start and end still contain a single base
  offset     = position - 1 # adjust for 1-start index
  exon.index = 1
  found      = FALSE

  for( exon.index in 1:length( widths ) ) {
    if( offset < widths[ exon.index ] ) {
      found  = TRUE
      break
    }
    offset   = offset - widths[ exon.index ]
  }

  if( !found && check.bounds ) {
    warning( paste( 'Fell off the end of transcript', transcript.id, 'looking for position', position ) )
    return( data.frame( start=NA, end=NA, chromosome_name=space, strand=strand, IN1=transcript.id, stringsAsFactors=FALSE ) )
  }
  else if( !found ) {
    offset = offset + widths[ exon.index ]
  }

  # ok, offset is the offset into the exon at exon.index
  if( strand > 0 ) {
    pos = exons$start[ exon.index ] + offset
    if( truncate ) {
      if( pos < exons$start[ 1 ] )     pos = exons$start[ 1 ]
      if( pos > tail( exons$end, 1 ) ) pos = tail( exons$end, 1 )
    }
  }
  else {
    pos = exons$end[ exon.index ] - offset
    if( truncate ) {
      if( pos < tail( exons$start, 1 ) ) pos = tail( exons$start, 1 )
      if( pos > exons$end[ 1 ] )         pos = exons$end[ 1 ]
    }
  }

  data.frame( start=pos, end=pos, chromosome_name=space, strand=strand, IN1=transcript.id, stringsAsFactors=FALSE )
}

transcriptCoordsToGenome = function( transcript.ids, position=1, as.vector=FALSE, check.bounds=TRUE, truncate=TRUE ) {
  transcript.ids = .get.correct.column( 'transcript', transcript.ids )

  if( is.null( transcript.ids ) ) {
    return( NULL )
  }

  exons = transcriptToExon( unique( transcript.ids ), as.vector='data.frame' )
  if( is.null( exons ) ) {
    return( NULL )
  }

  if( length( position ) == 1 ) {
    position = rep( position, length( transcript.ids ) )
  }
  else if( length( position ) != length( transcript.ids ) ) {
    stop( paste( 'Must have same number of positions as transcript.ids. (currently there are',
                length( position ), 'positions, and', length( transcript.ids ), 'transcript.ids)' ) )
  }

  data = do.call( 'rbind', lapply( seq_along( transcript.ids ), function( index ) {
    .single.transcript.coords.to.genome( transcript.ids[ index ],
                                         position[ index ],
                                         exons[ exons$IN1 == transcript.ids[ index ],, drop=FALSE ],
                                         check.bounds,
                                         truncate )
  } ) )

  if( dim( data )[2] != 5 ) {
    return( NULL )
  }

  if( as.vector == 'data.frame' ) {
    return( data )
  }

  if( as.vector ) {
    n           = data$IN1
    data        = data$start
    names(data) = n

    return( data )
  }

  data = data[ !is.na( data$start ),,drop=FALSE ]
  data = data[ !is.null( data$start ),,drop=FALSE ]

  if( dim( data )[1] == 0 ) {
    return( NULL )
  }

  colnames( data )[ colnames( data ) == "chromosome_name" ] = "space"
  data = as( data, 'RangedData' )

  if( .usegranges() ) {
    data = as( data, 'GRanges' )
  }

  return( data )
}

# -------------------------------------------------------------------------------
# Genome coords to Transcript

.genome.to.single.transcript.coords = function( position, transcript.id, exons, check.bounds ) {
  strand     = as.numeric( unique( exons$strand ) )
  space      = as.character( unique( exons$chromosome_name ) )

  if( dim( exons )[1] == 0 ) {
    return( data.frame( start=NA, end=NA, chromosome_name=NA, strand=NA, IN1=transcript.id, stringsAsFactors=FALSE ) )
  }

  exons      = exons[ order( exons$start, decreasing=strand < 0 ), ]
  widths     = exons$end - exons$start + 1

  if( strand > 0 ) {
    # Forward Strand
    if( position > max( exons$end ) ) {
      if( check.bounds ) {
        warning( paste( 'Fell off the end of', transcript.id, 'looking for position', position ) )
        return( data.frame( start=NA, end=NA, stable_id=transcript.id, coord.space='transcript', stringsAsFactors=FALSE ) )
      }
      pos = max( exons$end ) - position + sum( widths )
      return( data.frame( start=pos, end=pos, stable_id=transcript.id, coord.space='transcript', stringsAsFactors=FALSE ) )
    }
    if( position < min( exons$start ) ) {
      if( check.bounds ) {
        warning( paste( 'Fell off the start of', transcript.id, 'looking for position', position ) )
        return( data.frame( start=NA, end=NA, stable_id=transcript.id, coord.space='transcript', stringsAsFactors=FALSE ) )
      }
      pos = position - min( exons$start ) + 1
      return( data.frame( start=pos, end=pos, stable_id=transcript.id, coord.space='transcript', stringsAsFactors=FALSE ) )
    }

    # Otherwise, we are in the range of the transcript (need to check for intronic hit)
    exon.index = 1
    found      = FALSE
    offset     = 1

    for( exon.index in 1:length( widths ) ) {
      if( position >= exons$start[ exon.index ] && position <= exons$end[ exon.index ] ) {
        found  = TRUE
        break
      }
      offset = offset + widths[ exon.index ]
    }
    if( !found ) {
      # INTRONIC
      return( data.frame( start=NA, end=NA, stable_id=transcript.id, coord.space='transcript', stringsAsFactors=FALSE ) )
    }
    pos = offset + ( position - exons$start[ exon.index ] )
    return( data.frame( start=pos, end=pos, stable_id=transcript.id, coord.space='transcript', stringsAsFactors=FALSE ) )
  }
  else {
    # Reverse strand
    if( position < min( exons$start ) ) {
      if( check.bounds ) {
        warning( paste( 'Fell off the end of', transcript.id, 'looking for position', position ) )
        return( data.frame( start=NA, end=NA, stable_id=transcript.id, coord.space='transcript', stringsAsFactors=FALSE ) )
      }
      pos = min( exons$start ) - position + sum( widths )
      return( data.frame( start=pos, end=pos, stable_id=transcript.id, coord.space='transcript', stringsAsFactors=FALSE ) )
    }
    if( position > max( exons$end ) ) {
      if( check.bounds ) {
        warning( paste( 'Fell off the start of', transcript.id, 'looking for position', position ) )
        return( data.frame( start=NA, end=NA, stable_id=transcript.id, coord.space='transcript', stringsAsFactors=FALSE ) )
      }
      pos = max( exons$end ) - position + 1
      return( data.frame( start=pos, end=pos, stable_id=transcript.id, coord.space='transcript', stringsAsFactors=FALSE ) )
    }

    # Otherwise, we are in the range of the transcript (need to check for intronic hit)
    exon.index = 1
    found      = FALSE
    offset     = 1

    for( exon.index in 1:length( widths ) ) {
      if( position >= exons$start[ exon.index ] && position <= exons$end[ exon.index ] ) {
        found  = TRUE
        break
      }
      offset = offset + widths[ exon.index ]
    }
    if( !found ) {
      # INTRONIC
      return( data.frame( start=NA, end=NA, stable_id=transcript.id, coord.space='transcript', stringsAsFactors=FALSE ) )
    }
    pos = offset + ( exons$end[ exon.index ] - position )
    return( data.frame( start=pos, end=pos, stable_id=transcript.id, coord.space='transcript', stringsAsFactors=FALSE ) )
  }
}

genomeToTranscriptCoords = function( position, transcript.ids, as.vector=FALSE, check.bounds=TRUE ) {
  transcript.ids = as.character( .get.correct.column( 'transcript', transcript.ids ) )
  if( is.null( transcript.ids ) ) {
    return( NULL )
  }
  exons = transcriptToExon( unique( transcript.ids ), as.vector='data.frame' )
  if( is.null( exons ) ) {
    return( NULL )
  }

  if( length( position ) == 1 ) {
    position = rep( position, length( transcript.ids ) )
  }
  else if( length( position ) != length( transcript.ids ) ) {
    stop( paste( 'Must have the same number of positions as transcript.ids. (currently there are',
                length( position ), 'positions, and', length( exons ), 'transcript.ids)' ) )
  }

  data = do.call( 'rbind', lapply( seq_along( transcript.ids ), function( index ) {
    .genome.to.single.transcript.coords( position[ index ],
                                         transcript.ids[ index ],
                                         exons[ exons$IN1 == transcript.ids[ index ],, drop=FALSE ],
                                         check.bounds )
  } ) )

  # Return NULL for empty results  
  if( dim( data )[1] == 0 ) {
    return( NULL )
  }

  if( as.vector == FALSE ) { # RangedData object
    data = data[ !is.na( data$start ),,drop=FALSE ]
    data = data[ !is.null( data$start ),,drop=FALSE ]
    if( dim( data )[1] == 0 ) {
      return( NULL )
    }
    colnames( data )[ colnames( data ) == "stable_id" ] = "space"

    data = as( data, 'RangedData' )
    if( .usegranges() ) {
      data$strand = '*'
      data = as( data, 'GRanges' )
    }
  }
  else if( as.vector == TRUE ) {
    n             = data$stable_id
    data          = data$start
    names( data ) = n

    return( data )
  }
  # else, return data.frame
  data
}

# -------------------------------------------------------------------------------
# And then genome to protein

.single.protein.coords.to.genome = function( protein.id, transcript, position, as.vector, check.bounds, truncate ) {
  if( dim( transcript )[1] == 0 ) {
    warning( paste( 'No transcript located for protein', protein.id ) )
    return( data.frame( start=NA, end=NA, chromosome_name=NA, strand=NA, IN1=protein.id, position=position, stringsAsFactors=FALSE ) )
  }

  transcript.stable.id = .attr( transcript, 'stable_id' )
  local.coding.range   = transcriptToCodingRange( transcript.stable.id )
  space                = if( .usegranges() ) as.character( seqnames( local.coding.range ) ) else as.character( space( local.coding.range ) )
  strand               = if( .usegranges() ) strandAsInteger( local.coding.range ) else local.coding.range$strand

  if( check.bounds && position < 1 ) {
    warning( paste( 'Fell off the start of protein', protein.id, 'looking for position', position ) )
    return( data.frame( start=NA, end=NA, chromosome_name=space, strand=strand, IN1=protein.id, position=position, stringsAsFactors=FALSE ) )
  }

  if( strand > 0 ) {
    first.coding = genomeToTranscriptCoords( start( local.coding.range ), transcript.stable.id, as.vector=TRUE )
    start.phase  = .attr( local.coding.range, 'phase' )
    if( start.phase < 0 ) {
      start.phase = 0;
    }
    # Only causes an effect when no defined UTRs
    if( .attr( transcript, 'translation_start' ) > 1 ) {
      start.phase = 0
    }

    posl = first.coding + 3 * ( position - 1 ) - start.phase
    posr = first.coding + 3 * ( position - 1 ) - start.phase + 2
  }
  else {
    first.coding = genomeToTranscriptCoords( end( local.coding.range ), transcript.stable.id, as.vector=TRUE )
    start.phase  = .attr( local.coding.range, 'phase' )
    if( start.phase < 0 ) {
      start.phase = 0;
    }
    # Only causes an effect when no defined UTRs
    if( .attr( transcript, 'translation_start' ) > 1 ) {
      start.phase = 0
    }
    posr = first.coding + 3 * ( position - 1 ) - start.phase
    posl = first.coding + 3 * ( position - 1 ) - start.phase + 2
  }

  posl <- transcriptCoordsToGenome( transcript.stable.id, posl, as.vector=TRUE, check.bounds=FALSE, truncate )
  posr <- transcriptCoordsToGenome( transcript.stable.id, posr, as.vector=TRUE, check.bounds=FALSE, truncate )
  
  if( is.na( posl ) || is.na( posr ) ) {
    # We will have been warned about this earlier
    return( data.frame( start=NA, end=NA, chromosome_name=space, strand=strand, IN1=protein.id, position=position, stringsAsFactors=FALSE ) )
  }
  if( check.bounds && posl < ( start( local.coding.range ) - start.phase ) ) {
    warning( paste( 'Position', position, 'generates a position of', posl, 'which falls beyond the start of coding range', start( local.coding.range ), 'for', transcript.stable.id ) )
    return( data.frame( start=NA, end=NA, chromosome_name=space, strand=strand, IN1=protein.id, position=position, stringsAsFactors=FALSE ) )
  } 
  
  if( check.bounds && posr > end( local.coding.range ) + start.phase ) {
    warning( paste( 'Position', position, 'generates a position of', posr, 'which falls beyond the end of coding range', end( local.coding.range ), 'for', transcript.stable.id ) )
    return( data.frame( start=NA, end=NA, chromosome_name=space, strand=strand, IN1=protein.id, position=position, stringsAsFactors=FALSE ) )
  }
  data.frame( start=posl, end=posr, chromosome_name=space, strand=strand, IN1=protein.id, position=position, stringsAsFactors=FALSE )
}

proteinCoordsToGenome = function( protein.ids, position=1, as.vector=FALSE, check.bounds=TRUE, truncate=TRUE ) {
  protein.ids = as.character( .get.correct.column( 'protein', protein.ids ) )

  if( is.null( protein.ids ) ) {
    return( NULL )
  }

  transcripts = proteinToTranscript( unique(protein.ids), as.vector='data.frame')

  if( is.null( transcripts ) ) {
    return( NULL )
  }
  
  if( length( position ) == 1 ) {
    position = rep( position, length( protein.ids ) )
  }
  else if( length( position ) != length( protein.ids ) ) {
    stop( paste( 'Must have same number of positions as protein.ids. (currently there are',
                length( position ), 'positions, and', length( protein.ids ), 'protein.ids.\n' ) )
  }

  data = do.call( 'rbind', lapply( seq_along( protein.ids ), function( index ) {

    .single.protein.coords.to.genome( protein.ids[ index ],
                                      transcripts[ transcripts$IN1 == protein.ids[ index ], ],
                                      position[ index ],
                                      as.vector,
                                      check.bounds,
                                      truncate )
  } ) )

  if( dim( data )[2] != 6 || dim( data )[1] == 0 ) {
    return( NULL )
  }
  
  if( as.vector == 'data.frame' ) {
    return( data )
  }
  
  if( as.vector ) {
    n             = data$IN1
    data          = apply( data, 1, function( row ) {
                      as.numeric( if( as.numeric( row[ 'strand' ] ) > 0 ) row[ 'start' ] else row[ 'end' ] )
                    } )
    names( data ) = n

    return( data )
  }

  data = data[ !is.na( data$start ), ]

  if( dim( data )[1] == 0 ) {
    return( NULL )
  }
  
  colnames( data )[ colnames( data ) == "chromosome_name" ] = "space"

  data = as( data, 'RangedData' )

  if( .usegranges() ) {
    data = as( data, 'GRanges' )
  }

  data
}

.single.genome.to.protein.coords = function( protein.id, transcript, position, as.vector, check.bounds ) {
  local.coding.range = transcriptToCodingRange( transcript )
  utrl               = genomeToTranscriptCoords( start( local.coding.range ), transcript, as.vector=TRUE, check.bounds=check.bounds )
  utrr               = genomeToTranscriptCoords( end( local.coding.range ),   transcript, as.vector=TRUE, check.bounds=check.bounds )
  pos                = genomeToTranscriptCoords( position, transcript, as.vector=TRUE, check.bounds=check.bounds )
  space              = if( .usegranges() ) as.character( seqnames( local.coding.range ) ) else as.character( space( local.coding.range ) )
  strand             = if( .usegranges() ) strandAsInteger( local.coding.range ) else local.coding.range$strand

  if( is.na( pos ) ) {
    # Already been warned
    return( data.frame( codon=NA, frame=NA, stable_id=protein.id, coord.space='protein', stringsAsFactors=FALSE ) )
  }

  if(strand > 0) {
    start.phase = .attr( local.coding.range, 'phase' )
    if( start.phase < 0 ) {
      start.phase = 0
    }
    # Only causes an effect when no defined UTRs
    if( .attr( transcript, 'translation_start' ) > 1 ) {
      start.phase = 0
    }
    r = pos - utrl + start.phase
    if( check.bounds ) {
      if( pos > utrr ) {
        warning( paste( 'Fell off the end of', protein.id, 'looking for position', position ) )
        r = NA
      }
      else if( r < 0 ) {
        warning( paste( 'Fell off the start of', protein.id, 'looking for position', position ) )
        r = NA
      }
    }
  }
  else {
    start.phase = .attr( local.coding.range, 'phase' )
    if( start.phase < 0 ) {
      start.phase = 0;
    }
    # Only causes an effect when no defined UTRs
    if( .attr( transcript, 'translation_start' ) > 1 ) {
      start.phase = 0
    }
    r = pos - utrr + start.phase
    if( check.bounds ) {
      if( pos > utrl ) {
        warning( paste( 'Fell off the end of', protein.id, 'looking for position', position ) )
        r = NA
      }
      else if( r < 0 ) {
        warning( paste( 'Fell off the start of', protein.id, 'looking for position', position ) )
        r = NA
      }
    }
  }


  codon = floor( r / 3 ) + 1
  frame = r %% 3

  data.frame( codon=codon, frame=frame, stable_id=protein.id, coord.space='protein', stringsAsFactors=FALSE )
}

genomeToProteinCoords = function( position, protein.ids, as.vector=FALSE, check.bounds=TRUE ) {
  protein.ids = as.character( .get.correct.column( 'protein', protein.ids ) )
  if( is.null( protein.ids ) ) {
    return( NULL )
  }
  
  if( length( position ) == 1 ) {
    position = rep( position, length( protein.ids ) )
  }

  else if( length( position ) != length( protein.ids ) ) {
    stop( paste( 'Must have same number of positions as protein.ids. (currently there are',
                length( position ), 'positions, and', length( protein.ids ), 'protein.ids).\n' ) )
  }

  transcripts   = proteinToTranscript(protein.ids, as.vector='data.frame')
  if( is.null( transcripts ) ) {
    return( NULL )
  }

  data = do.call( 'rbind', lapply( seq_along( protein.ids ), function( index ) {
    .single.genome.to.protein.coords( protein.ids[ index ],
                                      transcripts[ transcripts$IN1 == protein.ids[ index ], ],
                                      position[ index ],
                                      as.vector,
                                      check.bounds )
  } ) )

  #NB - can return from the function from here
  if( dim( data )[1] == 0 ) {
    return( NULL )
  }

  if( as.vector == FALSE ) { # RangedData object
    data = data[ !is.na( data$codon ),,drop=FALSE ]
    if( dim( data )[1] == 0 ) {
      return( NULL )
    }
    colnames( data )[ colnames( data ) == "stable_id" ] = "space"
    colnames( data )[ colnames( data ) == "codon"     ] = "start"
    data = cbind( data, end=data$start )
    data = as( data, 'RangedData' )
    if( .usegranges() ) {
      data$strand = '*'
      data = as( data, 'GRanges' )
    }
  }
  else if( as.vector == TRUE ) {
    n             = data$stable_id
    data          = data$codon
    names( data ) = n
  }
  # else, return data.frame
  data
}
