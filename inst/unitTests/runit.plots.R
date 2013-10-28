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

# Crash if no genes in genome.plot
test.bug.ACBBUTIL33 = function() {
  data = tryCatch( annmapConnect( "mm-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'mm-test', so skipping this test." )
  }
  else {
    # Plot to the null device, we're just looking for a crash
    pdf( file = NULL )
    region_interest = RangedData( space='7', strand=-1, ranges=IRanges(start=3192050-5000, end=3202400+5000 ) )
    ngsBridgePlot( region_interest, genome.layout.weight=0.4, trace.clip=T, trace.pad=c(0.5,0), trace.bor = 'black', smoothing.function=NULL, probe.plot=NULL )
    dev.off()
    annmapDisconnect()
  }
}

test.bug.ANNMAP40 = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping this test." )
  }
  else {
    # Plot to the null device, we're just looking for a crash
    pdf( file = NULL )

    probe.data = "
probe_id                      name probe_hit_count   hit_id probe_id chromosome_id chromosome_name     start       end strand
1   878986 GTCCGCTTCTCGCCCAACAGCAGCA               1  3511083   878986         27527               5 180666528 180666552     -1
2  3447395 TTGTGTCCGCTTCTCGCCCAACAGC               1 16527596  3447395         27527               5 180666532 180666556     -1
3  1610367 TGTCTTGTGTCCGCTTCTCGCCCAA               1  7005397  1610367         27527               5 180666536 180666560     -1
4  1984140 GGTGTCTTGTGTCCGCTTCTCGCCC               1  8616662  1984140         27527               5 180666538 180666562     -1
"
    # Load the data in
    pcon = textConnection( probe.data )
    probes = read.table( pcon, header=TRUE )
    close( pcon )

    genomicPlot( RangedData( ranges=IRanges( 180666487, 180666582 ), space='5', strand=-1 ), highlights=probes )

    # Try with col added
    genomicPlot( RangedData( ranges=IRanges( 180666487, 180666582 ), space='5', strand=-1 ), highlights=cbind( probes, col='#000000' ) )

    # Try with important column missing
    ret = tryCatch( { genomicPlot( RangedData( ranges=IRanges( 180666487, 180666582 ), space='5', strand=-1 ), highlights=probes[ , colnames( probes ) != 'name' ] ) ; FALSE }, 
                    warning=function( w ) { TRUE } )
    print( ret )
    checkEquals( ret, TRUE, 'Should throw a warning with missing highlight fields' )

    dev.off()
    annmapDisconnect()
  }
}

test.bug.ANNMAP44 = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping this test." )
  }
  else {
    # Plot to the null device, we're just looking for a crash
    pdf( file = NULL )

    # This shouldn't crash, but did for ANNMAP44
    genomicPlot( exonDetails( 'ENSE00000919490' ) )
    dev.off()
    annmapDisconnect()
  }
}