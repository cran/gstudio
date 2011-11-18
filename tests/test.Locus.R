test.empty.locus <- function() {
	loc <- Locus()
	
	checkTrue( length(loc)==0 )
	checkTrue( is(loc,"Locus") )	
}

test.diploid.locus <- function() { 
	loc0 <- Locus()
	loc1 <- Locus( 1 )
	loc2 <- Locus( 1:2 )
	loc3 <- Locus( c("A","A") )
	
	checkTrue( is.na(loc0) )
	checkTrue( !is.na(loc1) )
	checkTrue( !is.na(loc2) )
	
	checkTrue( !is.heterozygote(loc0) )
	checkTrue( !is.heterozygote(loc1) )
	checkTrue( is.heterozygote(loc2)  )
	checkTrue( !is.heterozygote(loc3) )
	checkEqualsNumeric( length(loc0), 0 )
	checkEqualsNumeric( length(loc1), 1 )
	checkEqualsNumeric( length(loc2), 2 )
	
	checkEquals( as.vector( loc0, 1:4), rep(0,4) )
	checkEquals( as.vector( loc1, 1:4), c(1,0,0,0) )
	checkEquals( as.vector( loc2, 1:4), c(1,1,0,0) )
	
	checkEquals( as.character( loc0 ), NA )
	checkEquals( as.character( loc1 ), "1")
	checkEquals( as.character( loc2 ), "1:2")
	checkEquals( as.character( loc3 ), "A:A")	 
	
	checkEquals( loc3[1], "A" )
	loc3[2] <- "T"
	checkEquals( as.character(loc3), "A:T" )
}

test.ops.locus <- function() { 
	loc1 <- Locus( 1:2 )
	loc2 <- Locus( 1:2 )
	loc3 <- Locus( c("A","A") )

	checkTrue( loc1==loc2 )
	checkTrue( loc1!=loc3 )
}