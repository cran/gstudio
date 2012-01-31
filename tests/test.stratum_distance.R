test.stratum.distance <- function() {
	load("../data/araptus_attenuatus.rda")
	sonora <- araptus_attenuatus[araptus_attenuatus$Species=="CladeB",]
	P <- stratum.distance(sonora,"Pop",lat="Lat",lon="Long")
	
	coord.101 <- c(-110.574,27.9051)
	coord.102 <- c(-109.126,26.3801)
	coord.32  <- c(-109.327,26.6378)
	
	d.101.102 <- great.circle.distance( coord.101[1],coord.101[2],coord.102[1],coord.102[2])
	d.101.32  <- great.circle.distance( coord.101[1],coord.101[2],coord.32[1],coord.32[2])
	d.102.32  <- great.circle.distance( coord.102[1],coord.102[2],coord.32[1],coord.32[2])

	checkTrue( (d.101.102-P[1,2]) < 1 )
	checkTrue( (d.101.32 -P[1,3]) < 1 )
	checkTrue( (d.102.32 -P[2,3]) < 1 )
}


test.stratum.distance.subset <- function() {
	load("../data/araptus_attenuatus.rda")
	sonora <- araptus_attenuatus[araptus_attenuatus$Species=="CladeB",]

	P <- stratum.distance(sonora,"Pop",lat="Lat",lon="Long",subset=c("101","102") )
	
	coord.101 <- c(-110.574,27.9051)
	coord.102 <- c(-109.126,26.3801)
	
	d.101.102 <- great.circle.distance( coord.101[1],coord.101[2],coord.102[1],coord.102[2])
	
	checkTrue( (d.101.102-P[1,2]) < 1 )
	checkEquals( dim(P)[1], 2 ) 
	checkEquals( dim(P)[2], 2 )

}
