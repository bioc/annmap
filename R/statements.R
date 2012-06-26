
# --------------------------------WARNING---------------------------------
# This file has be automatically generated when this release of the xmap
# database was generated.  Any changes made to this file will be lost when
# the database is generated for the next version of Ensembl UNLESS the
# changes are fed back and placed in the generation script
# ------------------------------------------------------------------------



##################################################################
#                    GLOBAL UTILITY METHODS
#-----------------------------------------------------------------
#       This block will be posted once at the top of the file
##################################################################

.data.frame.to.rangeddata = function( x ) {
  cn = colnames( x )
  errs = c()
  if( !( 'chr' %in% cn ) && !( 'chromosome_name' %in% cn ) ) {
    errs = c( errs, 'data.frame in "x" needs a column called "chr" or "chromosome_name"' )
  }
  if( !( 'start' %in% cn ) ) {
    errs = c( errs, 'data.frame in "x" needs a column called "start"' )
  }
  if( !( 'end' %in% cn ) ) {
    errs = c( errs, 'data.frame in "x" needs a column called "end"' )
  }
  if( !( 'strand' %in% cn ) ) {
    errs = c( errs, 'data.frame in "x" needs a column called "strand"' )
  }
  if( length( errs ) > 0 ) {
    stop( paste( errs, collapse='\n' ) )
  }
  colnames(x)[colnames(x) == 'chr'] = 'space'
  colnames(x)[colnames(x) == 'chromosome_name'] = 'space'
  as( x, 'RangedData' )
}

.range.call = function( chr, start, end, strand, src, column, as.vector ) {
  .params = annmap:::.make.hash()
  .params$chr = chr
  .params$qstart = start
  .params$qstop = end
  .params$qstrand = strand
  r = .process( "range", src, .params )
  if( !( src %in% c( 'hit', 'prediction_exon' ) ) ) {
    r = .coerce( r, column, as.vector )
  }
  else if( src == 'hit' ) {
    r = .coerce( r, NULL, FALSE )
  }
  r
}

##################################################################
#               LOOKUP DATA AND FUNCTION DEFINITIONS
##################################################################

.xmap.queries = .make.hash()
.xmap.types = .make.hash()
# CDNA Probeset and Transcripts ar just probesets or Transcripts...  Hack this in...
.xmap.types$cdnaprobeset = "stable_id"
.xmap.types$cdnatranscript = "stable_id"
.xmap.types$array = "array_name"
.xmap.types$probeset = "stable_id"
.xmap.types$probe = "sequence"
.xmap.types$protein = "stable_id"
.xmap.types$domain = "stable_id"
.xmap.types$chromosome = "name"
.xmap.types$gene = "stable_id"
.xmap.types$transcript = "stable_id"
.xmap.types$exon = "stable_id"
.xmap.types$est_gene = "stable_id"
.xmap.types$est_transcript = "stable_id"
.xmap.types$est_exon = "stable_id"
.xmap.types$prediction_transcript = "stable_id"
.xmap.types$synonym = "synonym"
.xmap.queries$array.all = "CALL all_arrays(  )"
allArrays = function( as.vector=FALSE ) {
  .coerce( .process( "all", "array" ), .xmap.types$array, as.vector )
}
.xmap.queries$array.details = "CALL array_details( '${ids}' )"
arrayDetails = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( "details", "array", .params ), NULL, as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$array.to.probeset = "CALL array_to_probeset( '${array}' )"
arrayToProbeset = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'array', .params, 'probeset' ), .xmap.types$probeset, as.vector )
}
.xmap.queries$probeset.all = "CALL all_probesets( '${array}' )"
allProbesets = function( as.vector=FALSE ) {
  .coerce( .process( "all", "probeset" ), .xmap.types$probeset, as.vector )
}
.xmap.queries$probeset.details = "CALL probeset_details( '${array}', '${ids}' )"
probesetDetails = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( "details", "probeset", .params ), NULL, as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$probeset.range = "CALL probesets_in_range( '${array}', '${chr}', ${qstart}, ${qstop}, ${qstrand} )"

probesetInRange = function( x, ..., as.vector=FALSE ) { standardGeneric( 'probesetInRange' ) }
setGeneric( 'probesetInRange', probesetInRange )
setMethod(  'probesetInRange', signature( x='character' ),  function( x, start, end, strand, ..., as.vector=FALSE ) {
  if( length( x ) > 1 ) {
    if( length( unique( c( length( x ), length( start ), length( end ), length( strand ) ) ) ) != 1 ) {
      stop( 'All parameters "x", "start", "end", and "strand" must be the same length.' )
    }
    probesetInRange( data.frame( chr=x, start=start, end=end, strand=strand ), as.vector=as.vector )
  }
  else {
    if( is.na( strand ) || strand == 0 ) {
      if( as.vector == 'data.frame' || ( as.vector == FALSE && !.usegranges() ) ) {
        rbind( .range.call( x, start, end,  1, 'probeset', .xmap.types$probeset, as.vector=as.vector ),
               .range.call( x, start, end, -1, 'probeset', .xmap.types$probeset, as.vector=as.vector ) )
      }
      else {
        .fwd = .range.call( x, start, end,  1, 'probeset', .xmap.types$probeset, as.vector=as.vector )
        .rev = .range.call( x, start, end, -1, 'probeset', .xmap.types$probeset, as.vector=as.vector )
        if( is.null( .fwd ) ) .rev else if( is.null( .rev ) ) .fwd else c( .fwd, .rev )
      }
    }
    else {
      .range.call( x, start, end, strand, 'probeset', .xmap.types$probeset, as.vector=as.vector )
    }
  }
} )
setMethod(  'probesetInRange', signature( x='factor' ),     function( x, start, end, strand, ..., as.vector=FALSE ) {
  probesetInRange( as.character( x ), as.numeric( start ), as.numeric( end ), as.numeric( strand ), 'probeset', .xmap.types$probeset, as.vector=as.vector )
} )
setMethod(  'probesetInRange', signature( x='data.frame' ), function( x, as.vector=FALSE ) {
  probesetInRange( .data.frame.to.rangeddata( x ), as.vector=as.vector )
} )
setMethod(  'probesetInRange', signature( x='RangedData' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, probesetInRange, as.vector=as.vector ) 
} )
setMethod(  'probesetInRange', signature( x='NULL' ), function( x, as.vector=FALSE ) {
  return( NULL )
} )
setMethod(  'probesetInRange', signature( x='GRanges' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, probesetInRange, as.vector=as.vector )
} )

.xmap.queries$probeset.to.gene = "CALL probeset_to_gene( '${array}', '${ids}' )"
probesetToGene = function( ids, as.vector=FALSE, rm.unreliable=TRUE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  if( rm.unreliable ) {
    ids = unreliable( ids, exclude=TRUE )
    if( is.null( ids ) ) {
      return( NULL )
    }
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'probeset', .params, 'gene' ), .xmap.types$gene, as.vector )
}
.xmap.queries$probeset.to.transcript = "CALL probeset_to_transcript( '${array}', '${ids}' )"
probesetToTranscript = function( ids, as.vector=FALSE, rm.unreliable=TRUE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  if( rm.unreliable ) {
    ids = unreliable( ids, exclude=TRUE )
    if( is.null( ids ) ) {
      return( NULL )
    }
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'probeset', .params, 'transcript' ), .xmap.types$transcript, as.vector )
}
.xmap.queries$probeset.to.cdnatranscript = "CALL probeset_to_cdnatranscript( '${array}', '${ids}' )"
probesetToCdnatranscript = function( ids, as.vector=FALSE, rm.unreliable=TRUE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  if( rm.unreliable ) {
    ids = unreliable( ids, exclude=TRUE )
    if( is.null( ids ) ) {
      return( NULL )
    }
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'probeset', .params, 'cdnatranscript' ), .xmap.types$cdnatranscript, as.vector )
}
.xmap.queries$probeset.to.exon = "CALL probeset_to_exon( '${array}', '${ids}' )"
probesetToExon = function( ids, as.vector=FALSE, rm.unreliable=TRUE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  if( rm.unreliable ) {
    ids = unreliable( ids, exclude=TRUE )
    if( is.null( ids ) ) {
      return( NULL )
    }
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'probeset', .params, 'exon' ), .xmap.types$exon, as.vector )
}
.xmap.queries$probeset.to.est_gene = "CALL probeset_to_est_gene( '${array}', '${ids}' )"
probesetToEstGene = function( ids, as.vector=FALSE, rm.unreliable=TRUE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  if( rm.unreliable ) {
    ids = unreliable( ids, exclude=TRUE )
    if( is.null( ids ) ) {
      return( NULL )
    }
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'probeset', .params, 'est_gene' ), .xmap.types$est_gene, as.vector )
}
.xmap.queries$probeset.to.est_transcript = "CALL probeset_to_est_transcript( '${array}', '${ids}' )"
probesetToEstTranscript = function( ids, as.vector=FALSE, rm.unreliable=TRUE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  if( rm.unreliable ) {
    ids = unreliable( ids, exclude=TRUE )
    if( is.null( ids ) ) {
      return( NULL )
    }
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'probeset', .params, 'est_transcript' ), .xmap.types$est_transcript, as.vector )
}
.xmap.queries$probeset.to.est_exon = "CALL probeset_to_est_exon( '${array}', '${ids}' )"
probesetToEstExon = function( ids, as.vector=FALSE, rm.unreliable=TRUE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  if( rm.unreliable ) {
    ids = unreliable( ids, exclude=TRUE )
    if( is.null( ids ) ) {
      return( NULL )
    }
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'probeset', .params, 'est_exon' ), .xmap.types$est_exon, as.vector )
}
.xmap.queries$probeset.to.probe = "CALL probeset_to_probe( '${array}', '${ids}' )"
probesetToProbe = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'probeset', .params, 'probe' ), .xmap.types$probe, as.vector )
}
.xmap.queries$probeset.to.hit = "CALL probeset_to_hit( '${array}', '${ids}' )"
probesetToHit = function( ids, as.data.frame=FALSE, rm.unreliable=TRUE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  if( rm.unreliable ) {
    ids = unreliable( ids, exclude=TRUE )
    if( is.null( ids ) ) {
      return( NULL )
    }
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'probeset', .params, 'hit' ), 'keep_in1', as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$probeset.to.prediction_transcript = "CALL probeset_to_prediction_transcript( '${array}', '${ids}' )"
probesetToPredictionTranscript = function( ids, as.vector=FALSE, rm.unreliable=TRUE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  if( rm.unreliable ) {
    ids = unreliable( ids, exclude=TRUE )
    if( is.null( ids ) ) {
      return( NULL )
    }
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'probeset', .params, 'prediction_transcript' ), .xmap.types$prediction_transcript, as.vector )
}
.xmap.queries$probeset.to.protein = "CALL probeset_to_protein( '${array}', '${ids}' )"
probesetToProtein = function( ids, as.vector=FALSE, rm.unreliable=TRUE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  if( rm.unreliable ) {
    ids = unreliable( ids, exclude=TRUE )
    if( is.null( ids ) ) {
      return( NULL )
    }
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'probeset', .params, 'protein' ), .xmap.types$protein, as.vector )
}
.xmap.queries$probeset.to.domain = "CALL probeset_to_domain( '${array}', '${ids}' )"
probesetToDomain = function( ids, as.vector=FALSE, rm.unreliable=TRUE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  if( rm.unreliable ) {
    ids = unreliable( ids, exclude=TRUE )
    if( is.null( ids ) ) {
      return( NULL )
    }
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'probeset', .params, 'domain' ), .xmap.types$domain, as.vector )
}
.xmap.queries$probe.all = "CALL all_probes( '${array}' )"
allProbes = function( as.vector=FALSE ) {
  .coerce( .process( "all", "probe" ), .xmap.types$probe, as.vector )
}
.xmap.queries$probe.details = "CALL probe_details( '${array}', '${ids}' )"
probeDetails = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( "details", "probe", .params ), NULL, as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$probe.range = "CALL probes_in_range_quick( '${array}', '${chr}', ${qstart}, ${qstop}, ${qstrand} )"

probeInRange = function( x, ..., as.vector=FALSE ) { standardGeneric( 'probeInRange' ) }
setGeneric( 'probeInRange', probeInRange )
setMethod(  'probeInRange', signature( x='character' ),  function( x, start, end, strand, ..., as.vector=FALSE ) {
  if( length( x ) > 1 ) {
    if( length( unique( c( length( x ), length( start ), length( end ), length( strand ) ) ) ) != 1 ) {
      stop( 'All parameters "x", "start", "end", and "strand" must be the same length.' )
    }
    probeInRange( data.frame( chr=x, start=start, end=end, strand=strand ), as.vector=as.vector )
  }
  else {
    if( is.na( strand ) || strand == 0 ) {
      if( as.vector == 'data.frame' || ( as.vector == FALSE && !.usegranges() ) ) {
        rbind( .range.call( x, start, end,  1, 'probe', .xmap.types$probe, as.vector=as.vector ),
               .range.call( x, start, end, -1, 'probe', .xmap.types$probe, as.vector=as.vector ) )
      }
      else {
        .fwd = .range.call( x, start, end,  1, 'probe', .xmap.types$probe, as.vector=as.vector )
        .rev = .range.call( x, start, end, -1, 'probe', .xmap.types$probe, as.vector=as.vector )
        if( is.null( .fwd ) ) .rev else if( is.null( .rev ) ) .fwd else c( .fwd, .rev )
      }
    }
    else {
      .range.call( x, start, end, strand, 'probe', .xmap.types$probe, as.vector=as.vector )
    }
  }
} )
setMethod(  'probeInRange', signature( x='factor' ),     function( x, start, end, strand, ..., as.vector=FALSE ) {
  probeInRange( as.character( x ), as.numeric( start ), as.numeric( end ), as.numeric( strand ), 'probe', .xmap.types$probe, as.vector=as.vector )
} )
setMethod(  'probeInRange', signature( x='data.frame' ), function( x, as.vector=FALSE ) {
  probeInRange( .data.frame.to.rangeddata( x ), as.vector=as.vector )
} )
setMethod(  'probeInRange', signature( x='RangedData' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, probeInRange, as.vector=as.vector ) 
} )
setMethod(  'probeInRange', signature( x='NULL' ), function( x, as.vector=FALSE ) {
  return( NULL )
} )
setMethod(  'probeInRange', signature( x='GRanges' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, probeInRange, as.vector=as.vector )
} )

.xmap.queries$probe.to.hit = "CALL probe_to_hit( '${array}', '${ids}' )"
probeToHit = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'probe', .params, 'hit' ), 'keep_in1', as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$probe.to.probeset = "CALL probe_to_probeset( '${array}', '${ids}' )"
probeToProbeset = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'probe', .params, 'probeset' ), .xmap.types$probeset, as.vector )
}
.xmap.queries$protein.all = "CALL all_proteins(  )"
allProteins = function( as.vector=FALSE ) {
  .coerce( .process( "all", "protein" ), .xmap.types$protein, as.vector )
}
.xmap.queries$protein.details = "CALL protein_details( '${ids}' )"
proteinDetails = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( "details", "protein", .params ), NULL, as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$protein.range = "CALL proteins_in_range( '${chr}', ${qstart}, ${qstop}, ${qstrand} )"

proteinInRange = function( x, ..., as.vector=FALSE ) { standardGeneric( 'proteinInRange' ) }
setGeneric( 'proteinInRange', proteinInRange )
setMethod(  'proteinInRange', signature( x='character' ),  function( x, start, end, strand, ..., as.vector=FALSE ) {
  if( length( x ) > 1 ) {
    if( length( unique( c( length( x ), length( start ), length( end ), length( strand ) ) ) ) != 1 ) {
      stop( 'All parameters "x", "start", "end", and "strand" must be the same length.' )
    }
    proteinInRange( data.frame( chr=x, start=start, end=end, strand=strand ), as.vector=as.vector )
  }
  else {
    if( is.na( strand ) || strand == 0 ) {
      if( as.vector == 'data.frame' || ( as.vector == FALSE && !.usegranges() ) ) {
        rbind( .range.call( x, start, end,  1, 'protein', .xmap.types$protein, as.vector=as.vector ),
               .range.call( x, start, end, -1, 'protein', .xmap.types$protein, as.vector=as.vector ) )
      }
      else {
        .fwd = .range.call( x, start, end,  1, 'protein', .xmap.types$protein, as.vector=as.vector )
        .rev = .range.call( x, start, end, -1, 'protein', .xmap.types$protein, as.vector=as.vector )
        if( is.null( .fwd ) ) .rev else if( is.null( .rev ) ) .fwd else c( .fwd, .rev )
      }
    }
    else {
      .range.call( x, start, end, strand, 'protein', .xmap.types$protein, as.vector=as.vector )
    }
  }
} )
setMethod(  'proteinInRange', signature( x='factor' ),     function( x, start, end, strand, ..., as.vector=FALSE ) {
  proteinInRange( as.character( x ), as.numeric( start ), as.numeric( end ), as.numeric( strand ), 'protein', .xmap.types$protein, as.vector=as.vector )
} )
setMethod(  'proteinInRange', signature( x='data.frame' ), function( x, as.vector=FALSE ) {
  proteinInRange( .data.frame.to.rangeddata( x ), as.vector=as.vector )
} )
setMethod(  'proteinInRange', signature( x='RangedData' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, proteinInRange, as.vector=as.vector ) 
} )
setMethod(  'proteinInRange', signature( x='NULL' ), function( x, as.vector=FALSE ) {
  return( NULL )
} )
setMethod(  'proteinInRange', signature( x='GRanges' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, proteinInRange, as.vector=as.vector )
} )

.xmap.queries$protein.to.domain = "CALL protein_to_domain( '${ids}' )"
proteinToDomain = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'protein', .params, 'domain' ), .xmap.types$domain, as.vector )
}
.xmap.queries$protein.to.gene = "CALL protein_to_gene( '${ids}' )"
proteinToGene = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'protein', .params, 'gene' ), .xmap.types$gene, as.vector )
}
.xmap.queries$protein.to.transcript = "CALL protein_to_transcript( '${ids}' )"
proteinToTranscript = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'protein', .params, 'transcript' ), .xmap.types$transcript, as.vector )
}
.xmap.queries$protein.to.probeset = "CALL protein_to_probeset( '${array}', '${ids}' )"
proteinToProbeset = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'protein', .params, 'probeset' ), .xmap.types$probeset, as.vector )
}
.xmap.queries$domain.all = "CALL all_domains(  )"
allDomains = function( as.vector=FALSE ) {
  .coerce( .process( "all", "domain" ), .xmap.types$domain, as.vector )
}
.xmap.queries$domain.details = "CALL domain_details( '${ids}' )"
domainDetails = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( "details", "domain", .params ), NULL, as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$domain.range = "CALL domains_in_range( '${chr}', ${qstart}, ${qstop}, ${qstrand} )"

domainInRange = function( x, ..., as.vector=FALSE ) { standardGeneric( 'domainInRange' ) }
setGeneric( 'domainInRange', domainInRange )
setMethod(  'domainInRange', signature( x='character' ),  function( x, start, end, strand, ..., as.vector=FALSE ) {
  if( length( x ) > 1 ) {
    if( length( unique( c( length( x ), length( start ), length( end ), length( strand ) ) ) ) != 1 ) {
      stop( 'All parameters "x", "start", "end", and "strand" must be the same length.' )
    }
    domainInRange( data.frame( chr=x, start=start, end=end, strand=strand ), as.vector=as.vector )
  }
  else {
    if( is.na( strand ) || strand == 0 ) {
      if( as.vector == 'data.frame' || ( as.vector == FALSE && !.usegranges() ) ) {
        rbind( .range.call( x, start, end,  1, 'domain', .xmap.types$domain, as.vector=as.vector ),
               .range.call( x, start, end, -1, 'domain', .xmap.types$domain, as.vector=as.vector ) )
      }
      else {
        .fwd = .range.call( x, start, end,  1, 'domain', .xmap.types$domain, as.vector=as.vector )
        .rev = .range.call( x, start, end, -1, 'domain', .xmap.types$domain, as.vector=as.vector )
        if( is.null( .fwd ) ) .rev else if( is.null( .rev ) ) .fwd else c( .fwd, .rev )
      }
    }
    else {
      .range.call( x, start, end, strand, 'domain', .xmap.types$domain, as.vector=as.vector )
    }
  }
} )
setMethod(  'domainInRange', signature( x='factor' ),     function( x, start, end, strand, ..., as.vector=FALSE ) {
  domainInRange( as.character( x ), as.numeric( start ), as.numeric( end ), as.numeric( strand ), 'domain', .xmap.types$domain, as.vector=as.vector )
} )
setMethod(  'domainInRange', signature( x='data.frame' ), function( x, as.vector=FALSE ) {
  domainInRange( .data.frame.to.rangeddata( x ), as.vector=as.vector )
} )
setMethod(  'domainInRange', signature( x='RangedData' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, domainInRange, as.vector=as.vector ) 
} )
setMethod(  'domainInRange', signature( x='NULL' ), function( x, as.vector=FALSE ) {
  return( NULL )
} )
setMethod(  'domainInRange', signature( x='GRanges' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, domainInRange, as.vector=as.vector )
} )

.xmap.queries$domain.to.protein = "CALL domain_to_protein( '${ids}' )"
domainToProtein = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'domain', .params, 'protein' ), .xmap.types$protein, as.vector )
}
.xmap.queries$domain.to.gene = "CALL domain_to_gene( '${ids}' )"
domainToGene = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'domain', .params, 'gene' ), .xmap.types$gene, as.vector )
}
.xmap.queries$domain.to.transcript = "CALL domain_to_transcript( '${ids}' )"
domainToTranscript = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'domain', .params, 'transcript' ), .xmap.types$transcript, as.vector )
}
.xmap.queries$domain.to.probeset = "CALL domain_to_probeset( '${array}', '${ids}' )"
domainToProbeset = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'domain', .params, 'probeset' ), .xmap.types$probeset, as.vector )
}
.xmap.queries$chromosome.all = "CALL all_chromosomes(  )"
allChromosomes = function( as.vector=FALSE ) {
  .coerce( .process( "all", "chromosome" ), .xmap.types$chromosome, as.vector )
}
.xmap.queries$chromosome.details = "CALL chromosome_details( '${ids}' )"
chromosomeDetails = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( "details", "chromosome", .params ), NULL, as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$gene.all = "CALL all_genes(  )"
allGenes = function( as.vector=FALSE ) {
  .coerce( .process( "all", "gene" ), .xmap.types$gene, as.vector )
}
.xmap.queries$gene.details = "CALL gene_details( '${ids}' )"
geneDetails = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( "details", "gene", .params ), NULL, as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$gene.range = "CALL genes_in_range( '${chr}', ${qstart}, ${qstop}, ${qstrand} )"

geneInRange = function( x, ..., as.vector=FALSE ) { standardGeneric( 'geneInRange' ) }
setGeneric( 'geneInRange', geneInRange )
setMethod(  'geneInRange', signature( x='character' ),  function( x, start, end, strand, ..., as.vector=FALSE ) {
  if( length( x ) > 1 ) {
    if( length( unique( c( length( x ), length( start ), length( end ), length( strand ) ) ) ) != 1 ) {
      stop( 'All parameters "x", "start", "end", and "strand" must be the same length.' )
    }
    geneInRange( data.frame( chr=x, start=start, end=end, strand=strand ), as.vector=as.vector )
  }
  else {
    if( is.na( strand ) || strand == 0 ) {
      if( as.vector == 'data.frame' || ( as.vector == FALSE && !.usegranges() ) ) {
        rbind( .range.call( x, start, end,  1, 'gene', .xmap.types$gene, as.vector=as.vector ),
               .range.call( x, start, end, -1, 'gene', .xmap.types$gene, as.vector=as.vector ) )
      }
      else {
        .fwd = .range.call( x, start, end,  1, 'gene', .xmap.types$gene, as.vector=as.vector )
        .rev = .range.call( x, start, end, -1, 'gene', .xmap.types$gene, as.vector=as.vector )
        if( is.null( .fwd ) ) .rev else if( is.null( .rev ) ) .fwd else c( .fwd, .rev )
      }
    }
    else {
      .range.call( x, start, end, strand, 'gene', .xmap.types$gene, as.vector=as.vector )
    }
  }
} )
setMethod(  'geneInRange', signature( x='factor' ),     function( x, start, end, strand, ..., as.vector=FALSE ) {
  geneInRange( as.character( x ), as.numeric( start ), as.numeric( end ), as.numeric( strand ), 'gene', .xmap.types$gene, as.vector=as.vector )
} )
setMethod(  'geneInRange', signature( x='data.frame' ), function( x, as.vector=FALSE ) {
  geneInRange( .data.frame.to.rangeddata( x ), as.vector=as.vector )
} )
setMethod(  'geneInRange', signature( x='RangedData' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, geneInRange, as.vector=as.vector ) 
} )
setMethod(  'geneInRange', signature( x='NULL' ), function( x, as.vector=FALSE ) {
  return( NULL )
} )
setMethod(  'geneInRange', signature( x='GRanges' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, geneInRange, as.vector=as.vector )
} )

.xmap.queries$gene.to.transcript = "CALL gene_to_transcript( '${ids}' )"
geneToTranscript = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'gene', .params, 'transcript' ), .xmap.types$transcript, as.vector )
}
.xmap.queries$gene.to.exon = "CALL gene_to_exon( '${ids}' )"
geneToExon = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'gene', .params, 'exon' ), .xmap.types$exon, as.vector )
}
.xmap.queries$gene.to.exon_probeset = "CALL gene_to_exon_probeset( '${array}', '${ids}' )"
geneToExonProbeset = function( ids, as.vector=FALSE, probes.min=4 ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  r = .process( 'to', 'gene', .params, 'exon_probeset' )
  r[r[,'num_probes'] >= probes.min, , drop = FALSE]
 .coerce( r, 'probeset', as.vector )
}
.xmap.queries$gene.to.probeset = "CALL gene_to_probeset( '${array}', '${ids}' )"
geneToProbeset = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'gene', .params, 'probeset' ), .xmap.types$probeset, as.vector )
}
.xmap.queries$gene.to.synonym = "CALL gene_to_synonym( '${ids}' )"
geneToSynonym = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'gene', .params, 'synonym' ), .xmap.types$synonym, as.vector )
}
.xmap.queries$gene.to.protein = "CALL gene_to_protein( '${ids}' )"
geneToProtein = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'gene', .params, 'protein' ), .xmap.types$protein, as.vector )
}
.xmap.queries$gene.to.domain = "CALL gene_to_domain( '${ids}' )"
geneToDomain = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'gene', .params, 'domain' ), .xmap.types$domain, as.vector )
}
.xmap.queries$transcript.all = "CALL all_transcripts(  )"
allTranscripts = function( as.vector=FALSE ) {
  .coerce( .process( "all", "transcript" ), .xmap.types$transcript, as.vector )
}
.xmap.queries$transcript.details = "CALL transcript_details( '${ids}' )"
transcriptDetails = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( "details", "transcript", .params ), NULL, as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$transcript.range = "CALL transcripts_in_range( '${chr}', ${qstart}, ${qstop}, ${qstrand} )"

transcriptInRange = function( x, ..., as.vector=FALSE ) { standardGeneric( 'transcriptInRange' ) }
setGeneric( 'transcriptInRange', transcriptInRange )
setMethod(  'transcriptInRange', signature( x='character' ),  function( x, start, end, strand, ..., as.vector=FALSE ) {
  if( length( x ) > 1 ) {
    if( length( unique( c( length( x ), length( start ), length( end ), length( strand ) ) ) ) != 1 ) {
      stop( 'All parameters "x", "start", "end", and "strand" must be the same length.' )
    }
    transcriptInRange( data.frame( chr=x, start=start, end=end, strand=strand ), as.vector=as.vector )
  }
  else {
    if( is.na( strand ) || strand == 0 ) {
      if( as.vector == 'data.frame' || ( as.vector == FALSE && !.usegranges() ) ) {
        rbind( .range.call( x, start, end,  1, 'transcript', .xmap.types$transcript, as.vector=as.vector ),
               .range.call( x, start, end, -1, 'transcript', .xmap.types$transcript, as.vector=as.vector ) )
      }
      else {
        .fwd = .range.call( x, start, end,  1, 'transcript', .xmap.types$transcript, as.vector=as.vector )
        .rev = .range.call( x, start, end, -1, 'transcript', .xmap.types$transcript, as.vector=as.vector )
        if( is.null( .fwd ) ) .rev else if( is.null( .rev ) ) .fwd else c( .fwd, .rev )
      }
    }
    else {
      .range.call( x, start, end, strand, 'transcript', .xmap.types$transcript, as.vector=as.vector )
    }
  }
} )
setMethod(  'transcriptInRange', signature( x='factor' ),     function( x, start, end, strand, ..., as.vector=FALSE ) {
  transcriptInRange( as.character( x ), as.numeric( start ), as.numeric( end ), as.numeric( strand ), 'transcript', .xmap.types$transcript, as.vector=as.vector )
} )
setMethod(  'transcriptInRange', signature( x='data.frame' ), function( x, as.vector=FALSE ) {
  transcriptInRange( .data.frame.to.rangeddata( x ), as.vector=as.vector )
} )
setMethod(  'transcriptInRange', signature( x='RangedData' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, transcriptInRange, as.vector=as.vector ) 
} )
setMethod(  'transcriptInRange', signature( x='NULL' ), function( x, as.vector=FALSE ) {
  return( NULL )
} )
setMethod(  'transcriptInRange', signature( x='GRanges' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, transcriptInRange, as.vector=as.vector )
} )

.xmap.queries$transcript.to.gene = "CALL transcript_to_gene( '${ids}' )"
transcriptToGene = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'transcript', .params, 'gene' ), .xmap.types$gene, as.vector )
}
.xmap.queries$transcript.to.exon = "CALL transcript_to_exon( '${ids}' )"
transcriptToExon = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'transcript', .params, 'exon' ), .xmap.types$exon, as.vector )
}
.xmap.queries$transcript.to.exon_probeset = "CALL transcript_to_exon_probeset( '${array}', '${ids}' )"
transcriptToExonProbeset = function( ids, as.vector=FALSE, probes.min=4 ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  r = .process( 'to', 'transcript', .params, 'exon_probeset' )
  r[r[,'num_probes'] >= probes.min, , drop = FALSE]
 .coerce( r, .xmap.types$probeset, as.vector )
}
.xmap.queries$transcript.to.probeset = "CALL transcript_to_probeset( '${array}', '${ids}' )"
transcriptToProbeset = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'transcript', .params, 'probeset' ), .xmap.types$probeset, as.vector )
}
.xmap.queries$transcript.to.cdnaprobeset = "CALL transcript_to_cdnaprobeset( '${array}', '${ids}' )"
transcriptToCdnaprobeset = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'transcript', .params, 'cdnaprobeset' ), .xmap.types$cdnaprobeset, as.vector )
}
.xmap.queries$transcript.to.synonym = "CALL transcript_to_synonym( '${ids}' )"
transcriptToSynonym = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'transcript', .params, 'synonym' ), .xmap.types$synonym, as.vector )
}
.xmap.queries$transcript.to.protein = "CALL transcript_to_protein( '${ids}' )"
transcriptToProtein = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'transcript', .params, 'protein' ), .xmap.types$protein, as.vector )
}
.xmap.queries$transcript.to.domain = "CALL transcript_to_domain( '${ids}' )"
transcriptToDomain = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'transcript', .params, 'domain' ), .xmap.types$domain, as.vector )
}
.xmap.queries$exon.all = "CALL all_exons(  )"
allExons = function( as.vector=FALSE ) {
  .coerce( .process( "all", "exon" ), .xmap.types$exon, as.vector )
}
.xmap.queries$exon.details = "CALL exon_details( '${ids}' )"
exonDetails = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( "details", "exon", .params ), NULL, as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$exon.range = "CALL exons_in_range( '${chr}', ${qstart}, ${qstop}, ${qstrand} )"

exonInRange = function( x, ..., as.vector=FALSE ) { standardGeneric( 'exonInRange' ) }
setGeneric( 'exonInRange', exonInRange )
setMethod(  'exonInRange', signature( x='character' ),  function( x, start, end, strand, ..., as.vector=FALSE ) {
  if( length( x ) > 1 ) {
    if( length( unique( c( length( x ), length( start ), length( end ), length( strand ) ) ) ) != 1 ) {
      stop( 'All parameters "x", "start", "end", and "strand" must be the same length.' )
    }
    exonInRange( data.frame( chr=x, start=start, end=end, strand=strand ), as.vector=as.vector )
  }
  else {
    if( is.na( strand ) || strand == 0 ) {
      if( as.vector == 'data.frame' || ( as.vector == FALSE && !.usegranges() ) ) {
        rbind( .range.call( x, start, end,  1, 'exon', .xmap.types$exon, as.vector=as.vector ),
               .range.call( x, start, end, -1, 'exon', .xmap.types$exon, as.vector=as.vector ) )
      }
      else {
        .fwd = .range.call( x, start, end,  1, 'exon', .xmap.types$exon, as.vector=as.vector )
        .rev = .range.call( x, start, end, -1, 'exon', .xmap.types$exon, as.vector=as.vector )
        if( is.null( .fwd ) ) .rev else if( is.null( .rev ) ) .fwd else c( .fwd, .rev )
      }
    }
    else {
      .range.call( x, start, end, strand, 'exon', .xmap.types$exon, as.vector=as.vector )
    }
  }
} )
setMethod(  'exonInRange', signature( x='factor' ),     function( x, start, end, strand, ..., as.vector=FALSE ) {
  exonInRange( as.character( x ), as.numeric( start ), as.numeric( end ), as.numeric( strand ), 'exon', .xmap.types$exon, as.vector=as.vector )
} )
setMethod(  'exonInRange', signature( x='data.frame' ), function( x, as.vector=FALSE ) {
  exonInRange( .data.frame.to.rangeddata( x ), as.vector=as.vector )
} )
setMethod(  'exonInRange', signature( x='RangedData' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, exonInRange, as.vector=as.vector ) 
} )
setMethod(  'exonInRange', signature( x='NULL' ), function( x, as.vector=FALSE ) {
  return( NULL )
} )
setMethod(  'exonInRange', signature( x='GRanges' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, exonInRange, as.vector=as.vector )
} )

.xmap.queries$exon.to.gene = "CALL exon_to_gene( '${ids}' )"
exonToGene = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'exon', .params, 'gene' ), .xmap.types$gene, as.vector )
}
.xmap.queries$exon.to.transcript = "CALL exon_to_transcript( '${ids}' )"
exonToTranscript = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'exon', .params, 'transcript' ), .xmap.types$transcript, as.vector )
}
.xmap.queries$exon.to.probeset = "CALL exon_to_probeset( '${array}', '${ids}' )"
exonToProbeset = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'exon', .params, 'probeset' ), .xmap.types$probeset, as.vector )
}
.xmap.queries$est_gene.all = "CALL all_est_genes(  )"
allEstGenes = function( as.vector=FALSE ) {
  .coerce( .process( "all", "est_gene" ), .xmap.types$est_gene, as.vector )
}
.xmap.queries$est_gene.details = "CALL est_gene_details( '${ids}' )"
estGeneDetails = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( "details", "est_gene", .params ), NULL, as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$est_gene.range = "CALL est_genes_in_range( '${chr}', ${qstart}, ${qstop}, ${qstrand} )"

estGeneInRange = function( x, ..., as.vector=FALSE ) { standardGeneric( 'estGeneInRange' ) }
setGeneric( 'estGeneInRange', estGeneInRange )
setMethod(  'estGeneInRange', signature( x='character' ),  function( x, start, end, strand, ..., as.vector=FALSE ) {
  if( length( x ) > 1 ) {
    if( length( unique( c( length( x ), length( start ), length( end ), length( strand ) ) ) ) != 1 ) {
      stop( 'All parameters "x", "start", "end", and "strand" must be the same length.' )
    }
    estGeneInRange( data.frame( chr=x, start=start, end=end, strand=strand ), as.vector=as.vector )
  }
  else {
    if( is.na( strand ) || strand == 0 ) {
      if( as.vector == 'data.frame' || ( as.vector == FALSE && !.usegranges() ) ) {
        rbind( .range.call( x, start, end,  1, 'est_gene', .xmap.types$est_gene, as.vector=as.vector ),
               .range.call( x, start, end, -1, 'est_gene', .xmap.types$est_gene, as.vector=as.vector ) )
      }
      else {
        .fwd = .range.call( x, start, end,  1, 'est_gene', .xmap.types$est_gene, as.vector=as.vector )
        .rev = .range.call( x, start, end, -1, 'est_gene', .xmap.types$est_gene, as.vector=as.vector )
        if( is.null( .fwd ) ) .rev else if( is.null( .rev ) ) .fwd else c( .fwd, .rev )
      }
    }
    else {
      .range.call( x, start, end, strand, 'est_gene', .xmap.types$est_gene, as.vector=as.vector )
    }
  }
} )
setMethod(  'estGeneInRange', signature( x='factor' ),     function( x, start, end, strand, ..., as.vector=FALSE ) {
  estGeneInRange( as.character( x ), as.numeric( start ), as.numeric( end ), as.numeric( strand ), 'est_gene', .xmap.types$est_gene, as.vector=as.vector )
} )
setMethod(  'estGeneInRange', signature( x='data.frame' ), function( x, as.vector=FALSE ) {
  estGeneInRange( .data.frame.to.rangeddata( x ), as.vector=as.vector )
} )
setMethod(  'estGeneInRange', signature( x='RangedData' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, estGeneInRange, as.vector=as.vector ) 
} )
setMethod(  'estGeneInRange', signature( x='NULL' ), function( x, as.vector=FALSE ) {
  return( NULL )
} )
setMethod(  'estGeneInRange', signature( x='GRanges' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, estGeneInRange, as.vector=as.vector )
} )

.xmap.queries$est_gene.to.est_transcript = "CALL est_gene_to_est_transcript( '${ids}' )"
estGeneToEstTranscript = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'est_gene', .params, 'est_transcript' ), .xmap.types$est_transcript, as.vector )
}
.xmap.queries$est_gene.to.est_exon = "CALL est_gene_to_est_exon( '${ids}' )"
estGeneToEstExon = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'est_gene', .params, 'est_exon' ), .xmap.types$est_exon, as.vector )
}
.xmap.queries$est_gene.to.probeset = "CALL est_gene_to_probeset( '${array}', '${ids}' )"
estGeneToProbeset = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'est_gene', .params, 'probeset' ), .xmap.types$probeset, as.vector )
}
.xmap.queries$est_transcript.all = "CALL all_est_transcripts(  )"
allEstTranscripts = function( as.vector=FALSE ) {
  .coerce( .process( "all", "est_transcript" ), .xmap.types$est_transcript, as.vector )
}
.xmap.queries$est_transcript.details = "CALL est_transcript_details( '${ids}' )"
estTranscriptDetails = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( "details", "est_transcript", .params ), NULL, as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$est_transcript.range = "CALL est_transcripts_in_range( '${chr}', ${qstart}, ${qstop}, ${qstrand} )"

estTranscriptInRange = function( x, ..., as.vector=FALSE ) { standardGeneric( 'estTranscriptInRange' ) }
setGeneric( 'estTranscriptInRange', estTranscriptInRange )
setMethod(  'estTranscriptInRange', signature( x='character' ),  function( x, start, end, strand, ..., as.vector=FALSE ) {
  if( length( x ) > 1 ) {
    if( length( unique( c( length( x ), length( start ), length( end ), length( strand ) ) ) ) != 1 ) {
      stop( 'All parameters "x", "start", "end", and "strand" must be the same length.' )
    }
    estTranscriptInRange( data.frame( chr=x, start=start, end=end, strand=strand ), as.vector=as.vector )
  }
  else {
    if( is.na( strand ) || strand == 0 ) {
      if( as.vector == 'data.frame' || ( as.vector == FALSE && !.usegranges() ) ) {
        rbind( .range.call( x, start, end,  1, 'est_transcript', .xmap.types$est_transcript, as.vector=as.vector ),
               .range.call( x, start, end, -1, 'est_transcript', .xmap.types$est_transcript, as.vector=as.vector ) )
      }
      else {
        .fwd = .range.call( x, start, end,  1, 'est_transcript', .xmap.types$est_transcript, as.vector=as.vector )
        .rev = .range.call( x, start, end, -1, 'est_transcript', .xmap.types$est_transcript, as.vector=as.vector )
        if( is.null( .fwd ) ) .rev else if( is.null( .rev ) ) .fwd else c( .fwd, .rev )
      }
    }
    else {
      .range.call( x, start, end, strand, 'est_transcript', .xmap.types$est_transcript, as.vector=as.vector )
    }
  }
} )
setMethod(  'estTranscriptInRange', signature( x='factor' ),     function( x, start, end, strand, ..., as.vector=FALSE ) {
  estTranscriptInRange( as.character( x ), as.numeric( start ), as.numeric( end ), as.numeric( strand ), 'est_transcript', .xmap.types$est_transcript, as.vector=as.vector )
} )
setMethod(  'estTranscriptInRange', signature( x='data.frame' ), function( x, as.vector=FALSE ) {
  estTranscriptInRange( .data.frame.to.rangeddata( x ), as.vector=as.vector )
} )
setMethod(  'estTranscriptInRange', signature( x='RangedData' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, estTranscriptInRange, as.vector=as.vector ) 
} )
setMethod(  'estTranscriptInRange', signature( x='NULL' ), function( x, as.vector=FALSE ) {
  return( NULL )
} )
setMethod(  'estTranscriptInRange', signature( x='GRanges' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, estTranscriptInRange, as.vector=as.vector )
} )

.xmap.queries$est_transcript.to.est_gene = "CALL est_transcript_to_est_gene( '${ids}' )"
estTranscriptToEstGene = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'est_transcript', .params, 'est_gene' ), .xmap.types$est_gene, as.vector )
}
.xmap.queries$est_transcript.to.est_exon = "CALL est_transcript_to_est_exon( '${ids}' )"
estTranscriptToEstExon = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'est_transcript', .params, 'est_exon' ), .xmap.types$est_exon, as.vector )
}
.xmap.queries$est_transcript.to.probeset = "CALL est_transcript_to_probeset( '${array}', '${ids}' )"
estTranscriptToProbeset = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'est_transcript', .params, 'probeset' ), .xmap.types$probeset, as.vector )
}
.xmap.queries$est_exon.all = "CALL all_est_exons(  )"
allEstExons = function( as.vector=FALSE ) {
  .coerce( .process( "all", "est_exon" ), .xmap.types$est_exon, as.vector )
}
.xmap.queries$est_exon.details = "CALL est_exon_details( '${ids}' )"
estExonDetails = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( "details", "est_exon", .params ), NULL, as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$est_exon.range = "CALL est_exons_in_range( '${chr}', ${qstart}, ${qstop}, ${qstrand} )"

estExonInRange = function( x, ..., as.vector=FALSE ) { standardGeneric( 'estExonInRange' ) }
setGeneric( 'estExonInRange', estExonInRange )
setMethod(  'estExonInRange', signature( x='character' ),  function( x, start, end, strand, ..., as.vector=FALSE ) {
  if( length( x ) > 1 ) {
    if( length( unique( c( length( x ), length( start ), length( end ), length( strand ) ) ) ) != 1 ) {
      stop( 'All parameters "x", "start", "end", and "strand" must be the same length.' )
    }
    estExonInRange( data.frame( chr=x, start=start, end=end, strand=strand ), as.vector=as.vector )
  }
  else {
    if( is.na( strand ) || strand == 0 ) {
      if( as.vector == 'data.frame' || ( as.vector == FALSE && !.usegranges() ) ) {
        rbind( .range.call( x, start, end,  1, 'est_exon', .xmap.types$est_exon, as.vector=as.vector ),
               .range.call( x, start, end, -1, 'est_exon', .xmap.types$est_exon, as.vector=as.vector ) )
      }
      else {
        .fwd = .range.call( x, start, end,  1, 'est_exon', .xmap.types$est_exon, as.vector=as.vector )
        .rev = .range.call( x, start, end, -1, 'est_exon', .xmap.types$est_exon, as.vector=as.vector )
        if( is.null( .fwd ) ) .rev else if( is.null( .rev ) ) .fwd else c( .fwd, .rev )
      }
    }
    else {
      .range.call( x, start, end, strand, 'est_exon', .xmap.types$est_exon, as.vector=as.vector )
    }
  }
} )
setMethod(  'estExonInRange', signature( x='factor' ),     function( x, start, end, strand, ..., as.vector=FALSE ) {
  estExonInRange( as.character( x ), as.numeric( start ), as.numeric( end ), as.numeric( strand ), 'est_exon', .xmap.types$est_exon, as.vector=as.vector )
} )
setMethod(  'estExonInRange', signature( x='data.frame' ), function( x, as.vector=FALSE ) {
  estExonInRange( .data.frame.to.rangeddata( x ), as.vector=as.vector )
} )
setMethod(  'estExonInRange', signature( x='RangedData' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, estExonInRange, as.vector=as.vector ) 
} )
setMethod(  'estExonInRange', signature( x='NULL' ), function( x, as.vector=FALSE ) {
  return( NULL )
} )
setMethod(  'estExonInRange', signature( x='GRanges' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, estExonInRange, as.vector=as.vector )
} )

.xmap.queries$est_exon.to.est_gene = "CALL est_exon_to_est_gene( '${ids}' )"
estExonToEstGene = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'est_exon', .params, 'est_gene' ), .xmap.types$est_gene, as.vector )
}
.xmap.queries$est_exon.to.est_transcript = "CALL est_exon_to_est_transcript( '${ids}' )"
estExonToEstTranscript = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'est_exon', .params, 'est_transcript' ), .xmap.types$est_transcript, as.vector )
}
.xmap.queries$est_exon.to.probeset = "CALL est_exon_to_probeset( '${array}', '${ids}' )"
estExonToProbeset = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'est_exon', .params, 'probeset' ), .xmap.types$probeset, as.vector )
}
.xmap.queries$prediction_transcript.all = "CALL all_prediction_transcripts(  )"
allPredictionTranscripts = function( as.vector=FALSE ) {
  .coerce( .process( "all", "prediction_transcript" ), .xmap.types$prediction_transcript, as.vector )
}
.xmap.queries$prediction_transcript.details = "CALL prediction_transcript_details( '${ids}' )"
predictionTranscriptDetails = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( "details", "prediction_transcript", .params ), NULL, as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$prediction_transcript.range = "CALL prediction_transcripts_in_range( '${chr}', ${qstart}, ${qstop}, ${qstrand} )"

predictionTranscriptInRange = function( x, ..., as.vector=FALSE ) { standardGeneric( 'predictionTranscriptInRange' ) }
setGeneric( 'predictionTranscriptInRange', predictionTranscriptInRange )
setMethod(  'predictionTranscriptInRange', signature( x='character' ),  function( x, start, end, strand, ..., as.vector=FALSE ) {
  if( length( x ) > 1 ) {
    if( length( unique( c( length( x ), length( start ), length( end ), length( strand ) ) ) ) != 1 ) {
      stop( 'All parameters "x", "start", "end", and "strand" must be the same length.' )
    }
    predictionTranscriptInRange( data.frame( chr=x, start=start, end=end, strand=strand ), as.vector=as.vector )
  }
  else {
    if( is.na( strand ) || strand == 0 ) {
      if( as.vector == 'data.frame' || ( as.vector == FALSE && !.usegranges() ) ) {
        rbind( .range.call( x, start, end,  1, 'prediction_transcript', .xmap.types$prediction_transcript, as.vector=as.vector ),
               .range.call( x, start, end, -1, 'prediction_transcript', .xmap.types$prediction_transcript, as.vector=as.vector ) )
      }
      else {
        .fwd = .range.call( x, start, end,  1, 'prediction_transcript', .xmap.types$prediction_transcript, as.vector=as.vector )
        .rev = .range.call( x, start, end, -1, 'prediction_transcript', .xmap.types$prediction_transcript, as.vector=as.vector )
        if( is.null( .fwd ) ) .rev else if( is.null( .rev ) ) .fwd else c( .fwd, .rev )
      }
    }
    else {
      .range.call( x, start, end, strand, 'prediction_transcript', .xmap.types$prediction_transcript, as.vector=as.vector )
    }
  }
} )
setMethod(  'predictionTranscriptInRange', signature( x='factor' ),     function( x, start, end, strand, ..., as.vector=FALSE ) {
  predictionTranscriptInRange( as.character( x ), as.numeric( start ), as.numeric( end ), as.numeric( strand ), 'prediction_transcript', .xmap.types$prediction_transcript, as.vector=as.vector )
} )
setMethod(  'predictionTranscriptInRange', signature( x='data.frame' ), function( x, as.vector=FALSE ) {
  predictionTranscriptInRange( .data.frame.to.rangeddata( x ), as.vector=as.vector )
} )
setMethod(  'predictionTranscriptInRange', signature( x='RangedData' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, predictionTranscriptInRange, as.vector=as.vector ) 
} )
setMethod(  'predictionTranscriptInRange', signature( x='NULL' ), function( x, as.vector=FALSE ) {
  return( NULL )
} )
setMethod(  'predictionTranscriptInRange', signature( x='GRanges' ), function( x, as.vector=FALSE ) {
  annmapRangeApply( x, predictionTranscriptInRange, as.vector=as.vector )
} )

.xmap.queries$prediction_transcript.to.prediction_exon = "CALL prediction_transcript_to_prediction_exon( '${ids}' )"
predictionTranscriptToPredictionExon = function( ids ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .process( 'to', 'prediction_transcript', .params, 'prediction_exon' )
}
.xmap.queries$prediction_transcript.to.probeset = "CALL prediction_transcript_to_probeset( '${array}', '${ids}' )"
predictionTranscriptToProbeset = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'prediction_transcript', .params, 'probeset' ), .xmap.types$probeset, as.vector )
}
.xmap.queries$synonym.all = "CALL all_synonyms(  )"
allSynonyms = function( as.vector=FALSE ) {
  .coerce( .process( "all", "synonym" ), .xmap.types$synonym, as.vector )
}
.xmap.queries$synonym.details = "CALL synonym_details( '${ids}' )"
synonymDetails = function( ids, as.data.frame=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( "details", "synonym", .params ), NULL, as.vector=if( as.data.frame == TRUE ) 'data.frame' else as.data.frame )
}
.xmap.queries$synonym.to.gene = "CALL synonym_to_gene( '${ids}' )"
synonymToGene = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'synonym', .params, 'gene' ), .xmap.types$gene, as.vector )
}
.xmap.queries$synonym.to.transcript = "CALL synonym_to_transcript( '${ids}' )"
synonymToTranscript = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'synonym', .params, 'transcript' ), .xmap.types$transcript, as.vector )
}
.xmap.queries$synonym.to.est_gene = "CALL synonym_to_est_gene( '${ids}' )"
synonymToEstGene = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'synonym', .params, 'est_gene' ), .xmap.types$est_gene, as.vector )
}
.xmap.queries$synonym.to.est_transcript = "CALL synonym_to_est_transcript( '${ids}' )"
synonymToEstTranscript = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'synonym', .params, 'est_transcript' ), .xmap.types$est_transcript, as.vector )
}
.xmap.queries$symbol.to.gene = "CALL symbol_to_gene( '${ids}' )"
symbolToGene = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'symbol', .params, 'gene' ), .xmap.types$gene, as.vector )
}
.xmap.queries$symbol.to.transcript = "CALL symbol_to_transcript( '${ids}' )"
symbolToTranscript = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'symbol', .params, 'transcript' ), .xmap.types$transcript, as.vector )
}
.xmap.queries$symbol.to.est_gene = "CALL symbol_to_est_gene( '${ids}' )"
symbolToEstGene = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'symbol', .params, 'est_gene' ), .xmap.types$est_gene, as.vector )
}
.xmap.queries$symbol.to.est_transcript = "CALL symbol_to_est_transcript( '${ids}' )"
symbolToEstTranscript = function( ids, as.vector=FALSE ) {
  if( is.null( ids ) ) {
    return( NULL )
  }
  .params = .make.hash()
  .params$ids = ids
  .coerce( .process( 'to', 'symbol', .params, 'est_transcript' ), .xmap.types$est_transcript, as.vector )
}
