# This is a blank template for an RUnit test file

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

# An example test
test.template = function() {
  checkTrue( TRUE,        "True should be TRUE" )
  checkEquals( TRUE, TRUE, "And this should be ok too" )
}
