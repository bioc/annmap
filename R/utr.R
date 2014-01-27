.single.transcript.to.utr.range = function( transcript, end=c( 'both', '5', '3' ), on.translation.error ) {
  end = match.arg( end )
  space = as.character( transcript$chromosome_name )
  strand = as.numeric( transcript$strand )

  ret = data.frame( IN1=NULL, chromosome_name=NULL, start=NULL, end=NULL, strand=NULL, prime=NULL, phase=NULL, translated=NULL )

  if( is.na( transcript$translation_start_exon ) ) {
    if( end == 'both' ) {
      return( data.frame( IN1=c( transcript$stable_id, transcript$stable_id ),
                          chromosome_name=c( space, space ),
                          start=c( transcript$start, transcript$start ),
                          end=c( transcript$end, transcript$end ),
                          strand=c( strand, strand ),
                          prime=c( '5', '3' ),
                          phase=c( NA, NA ),
                          translated=c( FALSE, FALSE ),
                          stringsAsFactors=F ) )
    }
    else {
      return( data.frame( IN1=transcript$stable_id,
                          chromosome_name=space,
                          start=transcript$start,
                          end=transcript$end,
                          strand=strand,
                          prime=end,
                          phase=NA,
                          translated=F,
                          stringsAsFactors=F ) )
    }
  }

  exons = transcriptToExon( transcript$stable_id, as.vector='data.frame' )
  exons[,'sequence'] = NULL

  exons = exons[ order( exons$start, decreasing=strand < 0 ), ]

  sidx = 0
  eidx = 0

  for( idx in seq_along( exons$stable_id ) ) {
    if( exons$exon_id[ idx ] == transcript$translation_start_exon ) {
      sidx = idx
    }
    if( exons$exon_id[ idx ] == transcript$translation_end_exon ) {
      eidx = idx
    }
  }

  if( end %in% c( '5', 'both' ) ) {
    if( sidx == 1 && transcript$translation_start == 1 ) {
      # No 5' UTR
      ret = rbind( ret, data.frame( IN1=transcript$stable_id,
                                    chromosome_name=space,
                                    start=NA,
                                    end=NA,
                                    strand=strand,
                                    prime='5',
                                    phase=exons$phase[ sidx ],
                                    translated=T,
                                    stringsAsFactors=F ) )
    }
    else if( transcript$translation_start == 1 ) {
      if( strand > 0 ) {
        ret = rbind( ret, data.frame( IN1=transcript$stable_id,
                                      chromosome_name=space,
                                      start=transcript$start,
                                      end=exons$start[ sidx ] - 1,
                                      strand=strand,
                                      prime='5',
                                      phase=exons$phase[ sidx ],
                                      translated=T,
                                      stringsAsFactors=F ) )
      }
      else {
        ret = rbind( ret, data.frame( IN1=transcript$stable_id,
                                      chromosome_name=space,
                                      start=exons$end[ sidx ] + 1,
                                      end=transcript$end,
                                      strand=strand,
                                      prime='5',
                                      phase=exons$phase[ sidx ],
                                      translated=T,
                                      stringsAsFactors=F ) )
      }
    }
    else {
      if( strand > 0 ) {
        ret = rbind( ret, data.frame( IN1=transcript$stable_id,
                                      chromosome_name=space,
                                      start=transcript$start,
                                      end=exons$start[ sidx ] + transcript$translation_start - 2,
                                      strand=strand,
                                      prime='5',
                                      phase=exons$phase[ sidx ],
                                      translated=T,
                                      stringsAsFactors=F ) )
      }
      else {
        ret = rbind( ret, data.frame( IN1=transcript$stable_id,
                                      chromosome_name=space,
                                      start=exons$end[ sidx ] - transcript$translation_start + 2,
                                      end=transcript$end,
                                      strand=strand,
                                      prime='5',
                                      phase=exons$phase[ sidx ],
                                      translated=T,
                                      stringsAsFactors=F ) )
      }
    }
  }

  if( end %in% c( '3', 'both' ) ) {
    if( eidx == length( exons$stable_id ) && transcript$translation_end == exons$end[ eidx ] - exons$start[ eidx ] + 1 ) {
      # No 3' UTR
      ret = rbind( ret, data.frame( IN1=transcript$stable_id,
                                    chromosome_name=space,
                                    start=NA,
                                    end=NA,
                                    strand=strand,
                                    prime='3',
                                    phase=exons$end_phase[ eidx ],
                                    translated=T,
                                    stringsAsFactors=F ) )
    }
    else if( transcript$translation_end == exons$end[ eidx ] - exons$start[ eidx ] + 1 ) {
      # Just the last exons
      if( strand > 0 ) {
        ret = rbind( ret, data.frame( IN1=transcript$stable_id,
                                      chromosome_name=space,
                                      start=exons$end[ eidx ] + 1,
                                      end=transcript$end,
                                      strand=strand,
                                      prime='3',
                                      phase=exons$end_phase[ eidx ],
                                      translated=T,
                                      stringsAsFactors=F ) )
      }
      else {
        ret = rbind( ret, data.frame( IN1=transcript$stable_id,
                                      chromosome_name=space,
                                      start=transcript$start,
                                      end=exons$start[ eidx ] - 1,
                                      strand=strand,
                                      prime='3',
                                      phase=exons$end_phase[ eidx ],
                                      translated=T,
                                      stringsAsFactors=F ) )
      }
    }
    else {
      if( strand > 0 ) {
        ret = rbind( ret, data.frame( IN1=transcript$stable_id,
                                      chromosome_name=space,
                                      start=exons$start[ eidx ] + transcript$translation_end,
                                      end=transcript$end,
                                      strand=strand,
                                      prime='3',
                                      phase=exons$phase[ eidx ],
                                      translated=T,
                                      stringsAsFactors=F ) )
      }
      else {
        ret = rbind( ret, data.frame( IN1=transcript$stable_id,
                                      chromosome_name=space,
                                      start=transcript$start,
                                      end=exons$end[ eidx ] - transcript$translation_end,
                                      strand=strand,
                                      prime='3',
                                      phase=exons$phase[ eidx ],
                                      translated=T,
                                      stringsAsFactors=F ) )
      }
    }
  }
  # Check for negative width entries
  clamper = function( index, data, invalidfn ) {
    row = data[ index, ]
    if( ( !is.na( row$start ) && !is.na( row$end ) ) && ( row$end < row$start ) ) {
      invalidfn( paste( 'Translation falls off the ', row$prime, "' end of an exon for ", row$IN1, sep='' ) )
      row$start = NA
      row$end = NA
    }
    row
  }
  ret = do.call( 'rbind', lapply( seq_along( ret$IN1 ), clamper, data=ret, invalidfn=on.translation.error ) )
  return( ret )
}

transcriptToUtrRange = function( ids, end=c( 'both', '5', '3' ), as.data.frame=FALSE, on.translation.error=stop ) {
  ids = .get.correct.column( 'transcript', ids )
  if( any( duplicated( ids ) ) ) {
    warning( 'Duplicate ids detected.  Duplicates will be removed.' )
    ids = unique( ids )
  }
  if( is.null( ids ) ) {
    return( NULL )
  }
  ids = transcriptDetails( ids, as.data.frame=T )
  end = match.arg( end )
  .f = function( idx ) {
    .single.transcript.to.utr.range( ids[ idx, ], end, on.translation.error )
  }
  .data = lapply( seq_along( ids$stable_id ), .f )
  ret = do.call( 'rbind', .data )

  if( as.data.frame == TRUE ) {
    return( ret )
  }

  ret = ret[ !is.na( ret$start ), ]

  if( dim( ret )[1] == 0 ) {
    return( NULL )
  }
  colnames( ret )[ colnames( ret ) == "chromosome_name" ] = "space"

  ret = as( ret, 'RangedData' )
  if( .usegranges() ) {
    ret$strand = as.integer( ret$strand )
    as( ret, 'GRanges' )
  }
  else {
    ret
  }
}

transcriptToUtrExon = function( ids, end=c( 'both', '5', '3' ), as.vector=FALSE, on.translation.error=stop ) {
  ids = .get.correct.column( 'transcript', ids )
  if( any( duplicated( ids ) ) ) {
    warning( 'Duplicate ids detected.  Duplicates will be removed.' )
    ids = unique( ids )
  }
  ranges = transcriptToUtrRange( ids, end, on.translation.error=on.translation.error )
  exons  = transcriptToExon( ids )

  rslt = do.call( c, lapply( unique( exons$IN1 ), function( transcript ) {
    r = ranges[ ranges$IN1 == transcript, ]
    if( length( r ) == 0 ) {
      exons[ exons$IN1 == 'false', ]
    }
    else if( length( r ) == 1 ) {
      restrict( exons[ exons$IN1 == transcript, ], start=start( r ), end=end( r ) )
    }
    else {
      c( restrict( exons[ exons$IN1 == transcript, ], start=start( r[1] ), end=end( r[1] ) ),
         restrict( exons[ exons$IN1 == transcript, ], start=start( r[2] ), end=end( r[2] ) ) )
    }
  } ) )

  rslt$sequence = NULL
  if( length( rslt ) == 0 ) {
    return( NULL )
  }
  if( as.vector == 'data.frame' ) {
    strands = strandAsInteger( rslt )
    rslt = as.data.frame( rslt )
    colnames( rslt )[ colnames( rslt ) == "seqnames" ] = "chromosome_name"
    rslt$width = NULL
    rslt$strand = strands
  }
  else if( as.vector == TRUE ) {
    names = rslt$IN1
    rslt = rslt$stable_id
    names( rslt ) = names
  }
  rslt
}

transcriptToCodingRange = function( ids, end=c( 'both', '5', '3' ), as.data.frame=FALSE, on.translation.error=stop ) {
  ids = .get.correct.column( 'transcript', ids )
  if( any( duplicated( ids ) ) ) {
    warning( 'Duplicate ids detected.  Duplicates will be removed.' )
    ids = unique( ids )
  }
  if( is.null( ids ) ) {
    return( NULL )
  }
  end = match.arg( end )
  transcripts = transcriptDetails( ids, as.data.frame=TRUE )
  utr.transcripts = transcriptToUtrRange( ids, end='both', as.data.frame=TRUE, on.translation.error )
  .fn = function( idx ) {
    row = transcripts[ idx, ]
    utr = utr.transcripts[ utr.transcripts$IN1 == row$stable_id, ]
    phase = utr[ utr$prime == '5', ]$phase
    end.phase = utr[ utr$prime == '3', ]$phase
    if( end == '5' ) {
      utr = utr[ utr$prime == '5', ]
    }
    else if( end == '3' ) {
      utr = utr[ utr$prime == '3', ]
    }
    utr = utr[ !is.na( utr$start ), ]
    new = setdiff( IRanges( row$start, row$end ), IRanges( utr$start, utr$end ) )
    row$phase = phase
    row$end.phase = end.phase
    if( length( new ) == 0 ) {
      row$start = NA
      row$end = NA
    }
    else {
      row$start = start( new )
      row$end = end( new )
    }
    row
  }
  ret = do.call( 'rbind', lapply( seq_along( transcripts$stable_id ), .fn ) )
  if( as.data.frame == TRUE ) {
    return( ret )
  }
  ret = ret[ !is.na( ret$start ), ]
  colnames( ret )[ colnames( ret ) == "chromosome_name" ] = "space"
  ret = as( ret, 'RangedData' )
  if( .usegranges() ) {
    ret$strand = as.integer( ret$strand )
    as( ret, 'GRanges' )
  }
  else {
    ret
  }
}

transcriptToCodingExon = function( ids, end=c( 'both', '5', '3' ), as.vector=FALSE, on.translation.error=stop ) {
  ids = .get.correct.column( 'transcript', ids )
  if( any( duplicated( ids ) ) ) {
    warning( 'Duplicate ids detected.  Duplicates will be removed.' )
    ids = unique( ids )
  }
  ranges = transcriptToCodingRange( ids, end, on.translation.error=on.translation.error )
  exons  = transcriptToExon( ids )
  
  rslt = do.call( c, lapply( unique( exons$IN1 ), function( transcript ) {
    r = ranges[ ranges$IN1 == transcript, ]
    if( length( r ) == 0 ) {
      exons[ exons$IN1 == 'false', ]
    }
    else {
      out = restrict( exons[ exons$IN1 == transcript, ], start=start( r ), end=end( r ) )
    }
  } ) )

  rslt$sequence = NULL
  if( length( rslt ) == 0 ) {
    return( NULL )
  }
  if( as.vector == 'data.frame' ) {
    strands = strandAsInteger( rslt )
    rslt = as.data.frame( rslt )
    colnames( rslt )[ colnames( rslt ) == "seqnames" ] = "chromosome_name"
    rslt$width = NULL
    rslt$strand = strands
  }
  else if( as.vector == TRUE ) {
    names = rslt$IN1
    rslt = rslt$stable_id
    names( rslt ) = names
  }
  rslt
}

.nonIntronicLength = function( exons ) {
  sum( width( reduce( split( exons, exons$IN1 ) ) ) )
}

nonIntronicTranscriptLength = function( ids, end=c( 'none', 'both', '5', '3' ), on.translation.error=stop ) {
  end = match.arg( end )
  exons = if( end == 'none' ) transcriptToExon( ids ) else transcriptToCodingExon( ids, end=end, on.translation.error=on.translation.error )
  .nonIntronicLength( exons )
}

nonIntronicGeneLength = function( ids ) {
  .nonIntronicLength( geneToExon( ids ) )
}

.probeset.filtering = function( probesets, transcripts, end=c( 'both', '5', '3' ), fn, on.translation.error=stop ) {
  if( missing( probesets ) ) probesets = NULL
  if( missing( transcripts ) ) transcripts = NULL
  end = match.arg( end )
  
  probesets = .get.correct.column( 'probeset', probesets )
  transcripts = .get.correct.column( 'transcript', transcripts )
  
  if( is.null( probesets ) && is.null( transcripts ) ) {
    return( NULL )
  }
  else if( is.null( probesets ) ) {
    probesets = transcriptToProbeset( transcripts, as.vector=TRUE )
    if( is.null( probesets ) ) {
      return( NULL )
    }
  }
  else if( is.null( transcripts ) ) {
    transcripts = probesetToTranscript( probesets, as.vector='data.frame' )
    keep = !( is.na( transcripts$"translation_start" ) )
    transcripts = unique( transcripts[keep,]$'stable_id' )
    if( length( transcripts ) == 0 ) {
      return( NULL )
    }
  }
  else {
    tpts = probesetToTranscript( probesets, as.vector='data.frame' )
    keep = !( is.na( tpts$"translation_start" ) )
    tpts = tpts[keep,]
    keep = tpts$"stable_id" %in% transcripts
    tpts = tpts[ keep, ]
    if( dim( tpts )[1] == 0 ) {
      return( NULL )
    }
    transcripts = unique( tpts$'stable_id' )
  }
  probesets = unreliable( probesets, exclude=TRUE )
  ranges = fn( transcripts, end=end, FALSE, on.translation.error )
  if( is.null( ranges ) ) {
    return( NULL )
  }
  psts = annmapRangeApply( ranges, probesetInRange )
  if( is.null( psts ) ) {
    return( NULL )
  }
  psts = .get.correct.column( 'probeset', psts )
  keep = psts %in% probesets
  psts = psts[ keep ]
  if( length( psts ) == 0 ) {
    NULL
  }
  else {
    psts
  }
}

utrProbesets = function( probesets, transcripts, end=c( 'both', '5', '3' ), on.translation.error=stop ) {
  .probeset.filtering( probesets, transcripts, end, transcriptToUtrRange, on.translation.error )
}

codingProbesets = function( probesets, transcripts, end=c( 'both', '5', '3' ), on.translation.error=stop ) {
  .probeset.filtering( probesets, transcripts, end, transcriptToCodingRange, on.translation.error )
}
