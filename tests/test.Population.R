test.empty.population <- function() { 
	pop <- Population()
	
	checkEqualsNumeric( length(pop), 0 )
	checkEquals( names(pop), NULL )

}



test.full.population <- function() { 
	loci <- c( 	Locus( c(1,2) ),Locus( c(2,3) ),Locus( c(2,2) ),Locus( c(2,2) ),Locus( c(1,3) ),Locus( c(2,3) ),
				Locus( c(2,3) ),Locus( c(3,3) ),Locus( c(3,3) ),Locus( c(1,3) )	)
	strata <- c(rep("Cabo",5), rep("Loreto",5) )
	lat <- c(22.9270, 22.9443, 22.8263, 22.7751, 22.672, 22.3204, 22.2388, 22.3032, 22.1549, 22.2647)
	lon <- c(-109.767, -109.825, -109.955, -109.942, -110.081,-102.064, -102.123, -102.057, -101.931, -101.862)
	thePop <- Population( Pop=strata, lat=lat, lon=lon, TPI=loci )
	
	checkEqualsNumeric( length(thePop), 4)
	checkEqualsNumeric( dim(thePop)[1], 10)
	checkEqualsNumeric( dim(thePop)[2], 4)
	
	s <- summary(thePop)
	checkTrue( is(s,"table") )
	checkEqualsNumeric( length(s), 24 ) 
	checkTrue( all( names(thePop) == c("Pop","lat","lon","TPI") ) )

	# Getting Data From Columns
	checkEquals( thePop$Pop, strata )
	checkEquals( thePop[,1], thePop$Pop )

	# Getting Data From Rows
	row <- thePop[1,]
	checkEquals( row[1,1], "Cabo")
	checkEqualsNumeric( row[1,2], 22.9270)
	checkEqualsNumeric( row[1,3], -109.767 )
	checkEqualsNumeric( as.character(row[1,4]), "1:2" )
	
	# Slice Columns
	row <- thePop[1,2:3]
	checkEqualsNumeric( row[1,1], 22.9270)
	checkEqualsNumeric( row[1,2], -109.767 )
	
	# Slice Rows
	row <- thePop[1:4,2]
	checkEquals( row, c(22.9270, 22.9443, 22.8263, 22.7751) )

	# Logical Slice of Rows
	newPop <- thePop[ thePop$lat<22.5,]
	checkEqualsNumeric( length(newPop), 4)
	checkEqualsNumeric( dim(newPop)[1], 5)
	checkEquals( names( newPop ), names( thePop ) )
	
	# Slice Both
	row <- thePop[1:2,1:2]
	checkEqualsNumeric( length(row), 2 )
	checkEqualsNumeric( dim(row)[1], 2 )
	checkEquals(row[1,1],"Cabo")
	checkEquals(row[2,1],"Cabo")
	checkEqualsNumeric(row[1,2],22.9270)
	checkEqualsNumeric(row[2,2],22.9443)
	
	
	# Add Column by $
	thePop$New <- rep(-1,10)
	checkEqualsNumeric( dim(thePop)[1], 10 )
	checkEqualsNumeric( dim(thePop)[2], 5 )
	checkEquals( names(thePop)[5],"New")
	
	# Replace Column by $
	thePop$New <- rep(1,10)
	checkEqualsNumeric( sum(thePop$New), 10 )
	
	# Delete Column by $
	thePop$New <- NULL
	checkEqualsNumeric( dim(thePop)[1], 10 )
	checkEqualsNumeric( dim(thePop)[2], 4 )



	# Add Column by []
	thePop[,5] <- rep(-1,10)
	checkEqualsNumeric( dim(thePop)[1], 10 )
	checkEqualsNumeric( dim(thePop)[2], 5 )
	checkEquals( names(thePop)[5],"V5")
	
	# Replace Column by []
	thePop[,5] <- rep(1,10)
	checkEqualsNumeric( sum(thePop[,5]), 10 )
	
	# Delete Column by []
	thePop[,5] <- NULL
	checkEqualsNumeric( dim(thePop)[1], 10 )
	checkEqualsNumeric( dim(thePop)[2], 4 )
	
	# replace element by index [,]<-
	checkEquals( sum(thePop$Pop=="Loreto"), 5)
	thePop[1,1] <- "Loreto"
	checkEquals( sum(thePop$Pop=="Loreto"), 6)
	
	# replace elements by $ and then <-
	thePop$Pop[2] <- "Loreto"
	checkEquals( sum(thePop$Pop=="Loreto"), 7)
	
	# replace slice of column by $ and then []
	thePop$Pop[1:2] <- c("Cabo","Cabo")
	checkEquals( sum(thePop$Pop=="Loreto"), 5)

	# check assignment of single item to several rows
	thePop$Pop[5:10] <- "La Paz"
	checkEquals( sum(thePop$Pop=="Loreto"), 0)
	
	
	# check replacement of entire row by []
	thePop[1,] <- thePop[10,]
	checkEquals( sum(thePop$Pop=="La Paz"), 7)
	
	# check replace slice of row 
	reps <- list("Loreto",22.8,-109)
	thePop[1,1:3] <- reps
	checkEquals( thePop[1,1],"Loreto")
	checkEqualsNumeric( thePop[1,2], 22.8 )
	checkEqualsNumeric( thePop[1,3], -109 )
	
	
	# check column.names
	checkEquals(column.names(thePop,"character"),"Pop")
	checkEquals(column.names(thePop,"Locus"),"TPI")
	
	
	
	# check for indices
	checkEquals( indices(thePop,c("lat","lon")), 2:3 )
	checkTrue( length(indices(thePop,c("bob"))) == 0 )
	
}


test.frequencies.from.population <- function() {
	Pop <- c("A","A","B","B","B")
	TPI <- c(Locus(c(1,2)),Locus(c(2,3)),Locus(c(2,2)),Locus(c(2,2)),Locus(c(1,3)))
	PGM <- c(Locus(c(4,4)),Locus(c(4,3)),Locus(c(4,4)),Locus(c(3,4)),Locus(c(3,3)))
	thePop <- Population( Pop=Pop, TPI=TPI, PGM=PGM )
	freq <- allele.frequencies( thePop, "TPI" )
	checkTrue(class(freq$TPI)=="Frequencies")
	checkEqualsNumeric( get.frequencies( freq$TPI, 1), 2/10 )
	
	freqs <- allele.frequencies( thePop )
	checkEqualsNumeric( length(freqs), 2 )
	checkTrue( is(freqs[[1]],"Frequencies") )
	checkTrue( is(freqs[[2]],"Frequencies") )
	checkEquals(names(freqs), c("TPI","PGM") )
	checkEqualsNumeric(get.frequencies(freqs[[2]],4), 6/10 )
	
	
	# check partitionings
	subpops <- partition(thePop,"Pop")
	checkEqualsNumeric( length(subpops), 2 )
	checkTrue( is( subpops[[1]], "Population" ) )
	checkTrue( is( subpops[[2]], "Population" ) )
}

test.multivariate.loci.population <- function() {
	Pop <- c("A","A","B","B","B")
	TPI <- c(Locus(c(1,2)),Locus(c(2,3)),Locus(c(2,2)),Locus(c(2,2)),Locus(c(1,3)))
	PGM <- c(Locus(c(4,4)),Locus(c(4,3)),Locus(c(4,4)),Locus(c(3,4)),Locus(c(3,3)))
	thePop <- Population( Pop=Pop, TPI=TPI, PGM=PGM )	
	
	mv.locus <- multivariate.loci( thePop, "TPI" )

	checkTrue( is(mv.locus, "matrix" ) )
	checkEqualsNumeric( dim(mv.locus)[1], 5 )
	checkEqualsNumeric( dim(mv.locus)[2], 3 )
	checkEqualsNumeric( sum(mv.locus), 10 )
	checkTrue( all( mv.locus[1,] == c(1,1,0) ) )
	checkTrue( all( mv.locus[,3] == c(0,1,0,0,1) ) )
	
	
	mv.loci <- multivariate.loci( thePop )
	checkEqualsNumeric( dim(mv.loci)[1], 5 )
	checkEqualsNumeric( dim(mv.loci)[2], 5 )

	mv.locus.ploidy <- multivariate.loci(thePop,"TPI",ploidy=2 )
	checkTrue( is(mv.locus.ploidy, "matrix" ) )
	checkEqualsNumeric( dim(mv.locus.ploidy)[1], 5 )
	checkEqualsNumeric( dim(mv.locus.ploidy)[2], 3 )
	checkEqualsNumeric( sum(mv.locus.ploidy), 5 )
	checkTrue( all( mv.locus.ploidy[1,] == c(0.5,0.5,0) ) )
	checkTrue( all( mv.locus.ploidy[,3] == c(0,0.5,0,0,0.5) ) )

}





