# Testing for the utility functions of the package

if(FALSE) {
  library( "RUnit" )
  library( "annmap" )
}

###############################################################################
## Create a test environment
##
.test.env = new.env( hash=TRUE, parent=emptyenv() )

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

test.needs.array = function() {
  checkTrue( !annmap:::.needs.array( 'all', 'gene' ),           "needs array for 'all' actions should be false" )
  checkTrue( !annmap:::.needs.array( 'all', 'transcript' ),     "needs array for 'all' actions should be false" )
  checkTrue( !annmap:::.needs.array( 'all', 'exon' ),           "needs array for 'all' actions should be false" )

  checkTrue( !annmap:::.needs.array( 'details', 'gene' ),       "needs array for geneDetails should be false" )
  checkTrue( !annmap:::.needs.array( 'details', 'transcript' ), "needs array for transcriptDetails should be false" )
  checkTrue( !annmap:::.needs.array( 'details', 'exon' ),       "needs array for exonDetails should be false" )

  # This should be true, as despite it being an array, the array is not needed as a silent secondary parameter
  # The array in this case is the primary parameter (ids)
  checkTrue( !annmap:::.needs.array( 'details', 'array' ),      "needs array for arrayDetails should be false" )

  checkTrue( annmap:::.needs.array( 'details', 'probeset' ),      "anything with src probeset should need an array" )
  checkTrue( annmap:::.needs.array( 'details', 'probe' ),         "anything with src probe should need an array" )

  checkTrue( annmap:::.needs.array( 'to', 'gene', 'probeset' ),            "mapping to probeset destination should require an array" )
  checkTrue( annmap:::.needs.array( 'to', 'transcript', 'cdnaprobeset' ),  "mapping to cdnaprobeset destination should require an array" )
  checkTrue( annmap:::.needs.array( 'to', 'gene', 'probe' ),               "mapping to probe destination should require an array" )
  checkTrue( annmap:::.needs.array( 'to', 'probe', 'array' ),              "mapping to array destination should require an array" )
  checkTrue( annmap:::.needs.array( 'to', 'gene', 'exon_probeset' ),       "mapping to exon_probeset destination should require an array" )
}

test.make.params = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    # Check the quote escaping is working...
    ids = c( "a", "b", "c'", "hah';" )
    checkEquals( annmap:::.make.params( ids ), "a,b,c\\',hah\\';" )

    # Check the parameter splitting is working
    oldmax = annmapGetParam( "max.query" )
    annmapSetParam( max.query=20 )

    # Check with a list of short ids
    checkEquals( length( annmap:::.make.params( letters ) ), 3 )
    checkEquals( annmap:::.make.params( letters )[[1]], "a,b,c,d,e,f,g,h,i,j" )

    # Then check with ids that are bigger than the limit that we at least get one through per line
    values = c( "This is a long id (28 chars)", "Followed by another (30 chars)" )
    split = annmap:::.make.params( values )
    checkEquals( length( split ), 2, "with max.query set to 20, we should get each id on a new row" )
    checkEquals( split[[2]], values[2], "with max.query set to 20, we should get each id on a new row" )

    annmapSetParam( max.query=oldmax )

    # Get all the genes
    genes = allGenes( as.vector=TRUE )

    # Pick 5000 of them, and check the splitting works
    split = annmap:::.make.params( sample( genes, 5000 ) )
    checkEquals( 8, length( split ), 'Should be 8 elements in the list after the split' )
    checkTrue( is.list( split ), 'Should be a list when back from .make.params with multiple items' )
    checkTrue( all( unlist( lapply( split, function( vv ) is.character( vv ) ) ) ), 'Should be a list of character strings when back from .make.params with multiple items' )

    # Try with 10 genes
    split = annmap:::.make.params( sample( genes, 10 ) )
    checkEquals( 1, length( split ), 'Should be 1 element in the list after the split' )
    checkTrue( is.character( split ), 'Should be a character string when back from .make.params with one item' )
    annmapDisconnect()
  }
}

test.set.get.params = function() {
  # check single assignment
  annmapSetParam( testing=TRUE )
  checkTrue( annmapGetParam( "testing" ), "Testing should be equal to TRUE" )
  
  # And multiple assignment
  annmapSetParam( testing=FALSE, multiple="tim" )
  checkTrue( !annmapGetParam( "testing" ), "Testing should now be equal to FALSE" )
  checkEquals( annmapGetParam( "multiple" ), "tim" )  
  
  # And list parameters
  annmapSetParam( testing=c( 1, 2, 3 ) )
  checkEquals( length( annmapGetParam( "testing" ) ), 3, "Testing should have 3 elements" )

  annmapSetParam( testing=list( a=1, b=2, c=3 ) )
  checkEquals( annmapGetParam( "testing" )$b, 2, "Element 'b' should be 2" )

  # And functional parameters
  annmapSetParam( hi=function( a ) { paste( "Hi", a ) }, bye=function( a ) { paste( "Bye", a ) } )
  checkEquals( annmapGetParam( "hi" )( "Tim" ), "Hi Tim", "Testing should be a function returning a string" )
  checkEquals( annmapGetParam( "bye" )( "Tim" ), "Bye Tim", "Testing should be a function returning a string" )

  # A vector of functional parameters as a value
  fncs = c( function() { "function 1" }, function() { "function 2" } )
  annmapSetParam( fns=fncs )
  checkEquals( annmapGetParam( "fns" )[[1]](), "function 1", "Testing should be a function returning a string" )
  checkEquals( annmapGetParam( "fns" )[[2]](), "function 2", "Testing should be a function returning a string" )

  # And then a list of them
  fncs = list( a=function() { "function 1" }, b=function() { "function 2" } )
  annmapSetParam( fns=fncs )
  checkEquals( annmapGetParam( "fns" )[[1]](), "function 1", "Testing should be a function returning a string" )
  checkEquals( annmapGetParam( "fns" )[[2]](), "function 2", "Testing should be a function returning a string" )

  checkEquals( annmapGetParam( "fns" )$a(), "function 1", "Testing should be a function returning a string" )
  checkEquals( annmapGetParam( "fns" )$b(), "function 2", "Testing should be a function returning a string" )
}

test.array.type = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    # Check that we have selected a default array type (when we know that xmap.array.type is working)
    arr = annmap:::.xmap.internals$array
    checkTrue( !is.null( arr ), "We should have a default array type selected after xmap.connect" )
    
    # Then check that calling it with a stupid name fails
    checkException( arrayType( 'InvalidId' ), "Setting an invalid array type should fail", silent=TRUE )
    
    # And check we still have the same array type set after this failure
    checkEquals( arr, annmap:::.xmap.internals$array, "Same array should be set after failed call to annmapSetArray" )
    
    annmapDisconnect()
  }
}

test.rangeapply = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    # Need to add more tests for the other range queries
    checkTrue( is.vector( annmapRangeApply( symbolToGene( c( 'tp53', 'lama3' ), as.vector=F ), exonInRange, as.vector=T ) ),
      "Should return a vector for exons in range of 2 genes" )
    checkTrue( length(    annmapRangeApply( symbolToGene( c( 'tp53', 'lama3' ), as.vector=F ), exonInRange, as.vector=T ) ) > 0,
      "Should return a some results for exons in range of 2 genes" )
    checkTrue( is(        annmapRangeApply( symbolToGene( c( 'tp53', 'lama3' ), as.vector=F ), exonInRange, as.vector=F ), annmapGetParam( 'defaultclass' ) ),
      paste( "Should return a", annmapGetParam( 'defaultclass' ), "object for exons in range of 2 genes with as.vector=F" ) )
    d = annmapRangeApply( symbolToGene( c( 'tp53', 'lama3' ), as.vector=F ), exonInRange, as.vector=F )
    len = length( if( annmap:::.usegranges() ) d else d[[1]] )
    checkTrue( len > 2, paste( "Should return some", annmapGetParam( 'defaultclass' ), "rows for exons in range of 2 genes" ) )
  }
}

test.seqnames = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    checkTrue( all( as.character( seqnames( seqnamesToNCBI( symbolToGene( c( 'tp53', 'shh' ) ) ) ) ) == c( 'chr17', 'chr7' ) ), 'expected ncbi seqnames' )
  }
}

test.ANNMAP.97 = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    data = geneToExonProbeset( "ENSG00000166710", as.vector=TRUE )
    checkTrue( length( data ) > 0, 'geneToExonProbeset with as.vector=TRUE should return data' )
  }
}

test.ANNMAP.98 = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    # Shouldn't crash
    transcriptToTranslatedprobes( geneToTranscript( symbolToGene( 'hprt1' ) ) )
  }
}

test.ANNMAP.109 = function() {
  data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
  if( length( data ) == 1 ) {
    print( "Cannot find datasource 'hs-test', so skipping these tests." )
  }
  else {
    a = geneToSymbol( c( 'ENSG00000164690', 'ENSG00000171862' ) )
    b = geneDetails( c( 'ENSG00000164690', 'ENSG00000171862' ), as.data.frame=T )$symbol
    checkTrue( length( a ) > 0, 'geneToSymbol should return data' )
    checkTrue( length( b ) > 0, 'geneDetails$symbol should return data' )
    checkTrue( all( a == b ), 'returned symbols should be in the same order' )
  }
}

###############################################################################
## Stupid test to make sure its working...
##
test.reality <- function() {
  checkEqualsNumeric( 1, 1, "1 != 1?  PANIC!" )
}


