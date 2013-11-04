# Testing for the utility functions of the package

if(FALSE) {
  library( "RUnit" )
  library( "annmap" )
}

###############################################################################
## SetUp is called before each test method
##
.setUp = function() {
}

###############################################################################
## TearDown is called after each test method
##
.tearDown = function() {
    annmapDisconnect()
}

test.utr = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    gene = symbolToGene( "tp53" )
    probesets.vector = geneToProbeset( gene )
    probesets.frame  = geneToProbeset( gene, as.vector=FALSE )
    
    both.vector = utrProbesets( probesets.vector )
    both.frame  = utrProbesets( probesets.frame )
    checkEquals( length( both.vector ), length( both.frame ), "Should get the same results with Vector and Frame (both UTRs)" )
    
    five.vector = utrProbesets( probesets.vector, end="5" )
    five.frame  = utrProbesets( probesets.frame,  end="5" )
    checkEquals( length( five.vector ), length( five.frame ), "Should get the same results with Vector and Frame (5' end)" )
    
    transcripts = geneToTranscript( gene )
    singletranscript.vector = utrProbesets( probesets.vector, c( transcripts[1] ) )
    singletranscript.frame  = utrProbesets( probesets.frame,  c( transcripts[1] ) )
    checkEquals( length( singletranscript.vector ), length( singletranscript.frame ), "Should get the same results with Vector and Frame on a single Transcript (both UTRs)" )
    
    annmapDisconnect()
  }
}

test.yaoyong.utr.bug = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    # Should not get any intronic probesets back for this transcript
    yy.probesets = utrProbesets( transcriptToProbeset( 'ENST00000378357' ), 'ENST00000378357' )
    checkEquals( length( intronic( yy.probesets ) ), 0 )
  }
}

test.michal.utr.bug = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    # These hit exons inside the UTR exons (not just the exon where translation starts)
    checkEquals( utrProbesets( '3855309', 'ENST00000247005' ), '3855309' )
    checkEquals( utrProbesets( '3855308', 'ENST00000247005' ), '3855308' )
    checkEquals( utrProbesets( '3855297', 'ENST00000247005' ), '3855297' )
    checkEquals( utrProbesets( '3855316', 'ENST00000247005' ), '3855316' )
    # This probeset is inside the translated region
    checkTrue( is.null( utrProbesets( '3855295', 'ENST00000247005' ) ) )
  }
}

test.michal.utr.bug.two = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    g   = 'ENSG00000130283'
    ppp = geneToProbeset( g )
    ttt = geneToTranscript( g )
    checkEquals( length( utrProbesets( ppp, ttt ) ), 26 )
  }
}

# see bug XMAPCORE-3
test.michal.utr.bug.three = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    pp.t = exonic( transcriptToProbeset( 'ENST00000455250' ) )
    checkTrue( length( pp.t ) > 0 )
    checkTrue( is.null( utrProbesets( pp.t, "ENST00000455250" ) ) )
  }
}

test.ANNMAP118 = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    ttcr3 = transcriptToCodingRange( 'ENST00000463968', end='3' )
    checkEquals( length( ttcr3 ), 0 )

    ttcr5 = transcriptToCodingRange( 'ENST00000463968', end='5' )
    checkEquals( length( ttcr5 ), 0 )

    ttcrb = transcriptToCodingRange( 'ENST00000463968', end='both' )
    checkEquals( length( ttcrb ), 0 )
  }


}