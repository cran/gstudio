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
	
	loc4 <- Locus( "0", as.snp.minor=T )
	loc5 <- Locus( "1", as.snp.minor=T )
	loc6 <- Locus( "2", as.snp.minor=T )
	
	checkTrue( is.na(loc0) )
	checkTrue( !is.na(loc1) )
	checkTrue( !is.na(loc2) )
	checkTrue( !is.na(loc3) )
	checkTrue( !is.na(loc4) )
	checkTrue( !is.na(loc5) )
	checkTrue( !is.na(loc6) )
	
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
	checkEquals( as.vector( loc4, c("A","B")), c(2,0) )
	checkEquals( as.vector( loc5, c("A","B")), c(1,1) )
	checkEquals( as.vector( loc6, c("A","B")), c(0,2) )
	
	checkEquals( as.character( loc0 ), NA )
	checkEquals( as.character( loc1 ), "1")
	checkEquals( as.character( loc2 ), "1:2")
	checkEquals( as.character( loc3 ), "A:A")	 
	checkEquals( as.character( loc4 ), "A:A")
	checkEquals( as.character( loc5 ), "A:B")
	checkEquals( as.character( loc6 ), "B:B")
	
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