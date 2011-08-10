test.individual.distances <- function() {
	loci <- c( 	Locus( c(1,2) ),Locus( c(2,3) ),Locus( c(2,2) ),Locus( c(2,2) ),Locus( c(1,3) ),
				Locus( c(2,3) ),Locus( c(2,3) ),Locus( c(3,3) ),Locus( c(3,4) ),Locus( c(1,3) )	)
	thePop <- Population( TPI=loci )
	
	
	# jaccard Distance
	d.jaccard <- genetic.distance(thePop,mode="Jaccard")
	checkTrue( is(d.jaccard,"list") )
	checkEquals( names(d.jaccard), "TPI" )
	checkEqualsNumeric( d.jaccard$TPI[1,1], 0 )
	checkEqualsNumeric( d.jaccard$TPI[1,2], 2/3 )
	checkEqualsNumeric( d.jaccard$TPI[1,3], 1/2 )

	
	#bray-curtis distance
	d.bray <- genetic.distance(thePop,,mode="Bray")
	checkEqualsNumeric( d.bray$TPI[1,1], 0 )
	checkEqualsNumeric( d.bray$TPI[1,2], 0.5 )
	checkEqualsNumeric( d.bray$TPI[1,3], 0.5 )
	checkEqualsNumeric( d.bray$TPI[1,8], 1.0 )
	
	
	
	#amova distance
	d.amova <- genetic.distance(thePop,mode="AMOVA")
	checkEqualsNumeric( d.amova$TPI[1,1], 0 )
	checkEqualsNumeric( d.amova$TPI[1,2], 1 )
	checkEqualsNumeric( d.amova$TPI[1,3], 1 )
	checkEqualsNumeric( d.amova$TPI[1,8], 3 )
	checkEqualsNumeric( d.amova$TPI[3,8], 4 )
	checkEqualsNumeric( d.amova$TPI[1,9], 2 )

}



test.strata.distances <- function() {
	loci <- c( 	Locus( c(1,2) ),Locus( c(2,3) ),Locus( c(2,2) ),Locus( c(2,2) ),Locus( c(1,3) ),
				Locus( c(2,3) ),Locus( c(2,3) ),Locus( c(3,3) ),Locus( c(3,4) ),Locus( c(1,3) )	)
	strata <- c(rep("Cabo",5), rep("Loreto",5) )
	thePop <- Population( Pop=strata, TPI=loci )

	p1 <- c( 2/10, 6/10, 2/10, 0/10 )
	p2 <- c( 1/10, 2/10, 6/10, 1/10 )

	d.eucl <- genetic.distance( thePop, stratum="Pop", loci="TPI", mode="Euclidean" )	
	checkEqualsNumeric( d.eucl$TPI[1,1], 0 )
	checkEqualsNumeric( d.eucl$TPI[1,2], sqrt( sum( (p1-p2)^2 )) )
	
	d.cavalli <- genetic.distance( thePop, stratum="Pop", loci="TPI", mode="Cavalli" )
	checkEqualsNumeric( d.cavalli$TPI[1,1], 0 )
	checkEqualsNumeric( d.cavalli$TPI[1,2], sqrt( 2-2*sum(sqrt(p1*p2)) ) )
	

	d.nei <- genetic.distance( thePop, stratum="Pop", loci="TPI", mode="Nei" )
	top <- sum(p1*p2)
	bot <- sqrt(sum(p1^2) * sum(p2^2) )	
	nei <- -log( -1*log( top/bot))
	checkEqualsNumeric( d.nei$TPI[1,1], 0 )
	checkEqualsNumeric( d.nei$TPI[1,2], nei )
	
}
