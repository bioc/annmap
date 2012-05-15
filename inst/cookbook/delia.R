# This script builds the cookbook if the correct DB name is set up
# Or else does nothing, and leaves the PDF where it is...
#
# http://bit.ly/delia_smith

library( "annmap" )

data = tryCatch( annmapConnect( "hs-test" ), error=function(e) { FALSE } )
if( length( data ) == 1 ) {
	# The here's one I prepared earlier shortcut
	message( "Cannot find datasource 'hs-test', I'm not going to build the cookbook." )
} else {
	message( "Generating cookbook..." )
	# First, we'll disconnect, so the Rnw gets a fresh start
	annmapDisconnect()
	
	current.path = getwd()
	path <- file.path( getwd(), "..", "inst/cookbook" )
	setwd( path )
	# Parse the Rnw
	library(tools)
	utils::Sweave( 'cookbook.Rnw' )

	# Generate the PDF
	texi2dvi( 'cookbook.tex', pdf=T, clean=T )

	# Copy it to the docs
	file.copy( 'cookbook.pdf', file.path( path, '../..', 'vignettes' ), overwrite=T )

	# cleanup
	unlink( 'cookbook.pdf' )
	unlink( 'cookbook.tex' )
	unlink( 'bridge1.png' )
	unlink( 'bridge2.png' )
	unlink( 'bridge3.png' )
	unlink( 'bridge4.png' )
	unlink( 'bridge5.png' )
	unlink( 'fig1.pdf' )
	unlink( 'fig2.pdf' )
	unlink( 'fig3.pdf' )
	unlink( 'fig4.pdf' )
	unlink( 'fig5.pdf' )
	unlink( 'fig6.pdf' )
	unlink( 'fig7.pdf' )
	unlink( 'fig8.pdf' )
	unlink( 'fig9.pdf' )
	unlink( 'fig10.pdf' )

	setwd( current.path )
}

