test.minus.mom <- function() { 
	m1 <- Locus( c("A","A") )
	m2 <- Locus( c("A","B") )
	o1 <- Locus( c("A","A") )
	o2 <- Locus( c("A","B") )
	o3 <- Locus( c("B","B") )

	mm11 <- o1-m1
	mm12 <- o1-m2
	
	checkEquals( length(o1-m1), 1 )
	checkEquals( length(o1-m2), 1 )
	checkEquals( length(o2-m2), 2 )

}
