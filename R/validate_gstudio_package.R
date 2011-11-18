##############################################################################
# 					        Runs Test Suite on Package                       #
#                                                                            #
# 						    R.J. Dyer <rjdyer@@vcu.edu>                      #
#                                                                            #
##############################################################################


#' Verify test conditions for package components
#'
#' @name validate_gstudio_tests
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>


validate_gstudio_package <- function() {
	require(RUnit)
	rm(list=ls())
		
	files_to_exclude = c("validate_gstudio_package.R","gstudio-package.R")

	source_files <- list.files( pattern="\\w+\\.R$")

	for( file in source_files ){
		if( !(file %in% files_to_exclude ) && !("Class" %in% file) ) {
			cat("Including File: ",file,"\n")
			source(file)
		}
	}
	

	test.suite <- defineTestSuite("gstudio_testsuite",
	dirs=file.path("../tests"),
	testFileRegexp='^test\\.\\w+\\.R')
	result <- runTestSuite( test.suite )
	printTextProtocol( result )

}


