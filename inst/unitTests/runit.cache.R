# Testing for the cache functions of the package

if(FALSE) {
  library( "RUnit" )
  library( "annmap" )
}

###############################################################################
## SetUp is called before each test method
##
.setUp = function() {
  cache.location = tempdir()
  annmap:::.set.cache.root( cache.location )
  annmapClearCache()
}

###############################################################################
## TearDown is called after each test method
##
.tearDown = function() {
  annmapClearCache()
}

test.cache = function() {
  object = letters
  key = "lettersKey"
  
  # Check something doesnt exist
  checkTrue( !annmap:::.cache.exists( key ), "Cached item seems to exist before I created it" )
  
  # Then add it in to the cache
  annmap:::.cache.store( key, object )
  
  # Then check it does exist
  checkTrue( annmap:::.cache.exists( key ), "Cached item not found after I created it" )

  # Then check its value
  checkEquals( annmap:::.cache.retrieve( key ), object, "Got a different object out of the cache from the one I put in there" )
  
  # Then clear the cache
  annmapClearCache()
  
  # Then check it doesnt exist
  checkTrue( !annmap:::.cache.exists( key ), "Cached item seems to exist...but I just cleared the cache" )
}
