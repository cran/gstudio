test.frequencies <- function() {
	loci <- c( 	Locus( c(1,2) ),Locus( c(2,3) ),Locus( c(2,2) ),Locus( c(2,2) ),Locus( c(1,3) ),
				Locus( c(2,3) ),Locus( c(2,3) ),Locus( c(3,3) ),Locus( c(3,3) ),Locus( c(1,3) )	)
				
	freq <- Frequencies( loci )

	f <- c(3/20, 8/20, 9/20 )
	h.o <- 6/10
	h.e <- 1.0 - sum( f^2 )
	
	checkEqualsNumeric( length(freq), 3 )
	checkEqualsNumeric( length(names(freq)), 3 )
	checkEqualsNumeric( get.frequencies(freq,1), f[1])
	checkEqualsNumeric( get.frequencies(freq,3), f[3])
	
	
	
	checkEqualsNumeric( length( get.frequencies(freq)), 3 )
	
	checkEqualsNumeric( ho(freq), h.o )
	checkEqualsNumeric( he(freq), h.e )
	
	checkEquals( class( summary(freq) ), "summary.gstudio" )
	
	
}


