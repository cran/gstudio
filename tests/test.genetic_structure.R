test.singlelocus.structure <- function() {
	loci <- c( 	Locus( c(1,2) ),Locus( c(2,3) ),Locus( c(2,2) ),Locus( c(2,2) ),Locus( c(1,3) ),
				Locus( c(2,3) ),Locus( c(2,3) ),Locus( c(3,3) ),Locus( c(3,4) ),Locus( c(1,3) )	)
	strata <- c(rep("Cabo",5), rep("Loreto",5) )
	thePop <- Population( Pop=strata, TPI=loci )
	
	p1 <- c(2/10,6/10,2/10,0/10)
	p2 <- c(1/10,2/10,6/10,1/10)
	p.tot <-c(3/20,8/20,8/20,1/20)
	
	k <- 2
	ht <- 1- sum( p.tot^2 )
	hs <- ((1-sum(p1^2))+(1-sum(p2^2)))/2
	n.harmonic <- 1/mean(1/table(as.factor(strata)))
	hs.estimated <- (2*n.harmonic)/(2*n.harmonic-1) * hs
	ht.estimated <- ht + hs.estimated/(2*k*n.harmonic)
	gst.calc <- 1-hs.estimated/ht.estimated
	gst <- genetic.structure(thePop,stratum="Pop",loci="TPI",mode="Gst",num.perm=0)
	
	checkTrue( is(gst,"genetic.structure") )
	checkEqualsNumeric( gst$estimate[[1]], gst.calc )
	checkEquals(gst$mode,"Gst")
	checkEquals(gst$loci,"TPI")
	
	
	#gstprime
	gst.prime.calc <- ((1-hs.estimated/ht.estimated) * (k-1+hs.estimated) )
	gst.prime.calc <- gst.prime.calc / ((k-1)*(1-hs.estimated))
	gst.prime <- genetic.structure(thePop,stratum="Pop",loci="TPI",mode="Gst.prime",num.perm=0)
	checkTrue( is(gst,"genetic.structure") )
	checkEqualsNumeric( gst.prime$estimate[[1]], gst.prime.calc )
	checkEquals(gst.prime$mode,"Gst.prime")
	checkEquals(gst.prime$loci,"TPI")
	
	
	#Dest
	Dest.calc <- ((ht.estimated-hs.estimated) / (1-hs.estimated))
	Dest.calc <- Dest.calc / (k/(k-1))
	dest <- genetic.structure(thePop,stratum="Pop",loci="TPI",mode="Dest",num.perm=0)
	checkTrue( is(gst,"genetic.structure") )
	checkEqualsNumeric( dest$estimate[[1]], Dest.calc )
	checkEquals(dest$mode,"Dest")
	checkEquals(dest$loci,"TPI")	
}



test.singlelocus.no.shared.structure <- function() {
	loci <- c( 	Locus( c(4,4) ),Locus( c(4,3) ),Locus( c(3,3) ),Locus( c(3,3) ),Locus( c(4,3) ),
				Locus( c(2,1) ),Locus( c(2,1) ),Locus( c(1,1) ),Locus( c(1,2) ),Locus( c(1,1) )	)
	strata <- c(rep("Cabo",5), rep("Loreto",5) )
	thePop <- Population( Pop=strata, PGM=loci )
	
	p1 <- c(0/10,0/10,6/10,4/10)
	p2 <- c(7/10,3/10,0/10,0/10)
	p.tot <-c(7/20,3/20,6/20,4/20)
	
	k <- 2
	ht <- 1- sum( p.tot^2 )
	hs <- ((1-sum(p1^2))+(1-sum(p2^2)))/2
	n.harmonic <- 1/mean(1/table(as.factor(strata)))
	hs.estimated <- (2*n.harmonic)/(2*n.harmonic-1) * hs
	ht.estimated <- ht + hs.estimated/(2*k*n.harmonic)
	gst.calc <- 1-hs.estimated/ht.estimated
	gst <- genetic.structure(thePop,stratum="Pop",loci="PGM",mode="Gst",num.perm=0)
	
	checkTrue( is(gst,"genetic.structure") )
	checkEqualsNumeric( gst$estimate[[1]], gst.calc )
	checkEquals(gst$mode,"Gst")
	checkEquals(gst$loci,"PGM")
	
	
	#gstprime
	gst.prime.calc <- ((1-hs.estimated/ht.estimated) * (k-1+hs.estimated) )
	gst.prime.calc <- gst.prime.calc / ((k-1)*(1-hs.estimated))
	gst.prime <- genetic.structure(thePop,stratum="Pop",loci="PGM",mode="Gst.prime",num.perm=0)
	checkTrue( is(gst,"genetic.structure") )
	checkEqualsNumeric( gst.prime$estimate[[1]], gst.prime.calc )
	checkEquals(gst.prime$mode,"Gst.prime")
	checkEquals(gst.prime$loci,"PGM")
	
	
	#Dest
	Dest.calc <- ((ht.estimated-hs.estimated) / (1-hs.estimated))
	Dest.calc <- Dest.calc / (k/(k-1))
	dest <- genetic.structure(thePop,stratum="Pop",loci="PGM",mode="Dest",num.perm=0)
	checkTrue( is(gst,"genetic.structure") )
	checkEqualsNumeric( dest$estimate[[1]], Dest.calc )
	checkEquals(dest$mode,"Dest")
	checkEquals(dest$loci,"PGM")	
}





test.multilocus.structure <- function() {

	loci <- c( 	Locus( c(1,2) ),Locus( c(2,3) ),Locus( c(2,2) ),Locus( c(2,2) ),Locus( c(1,3) ),
				Locus( c(2,3) ),Locus( c(2,3) ),Locus( c(3,3) ),Locus( c(3,4) ),Locus( c(1,3) )	)
	loci2<- c( 	Locus( c(4,4) ),Locus( c(4,3) ),Locus( c(3,3) ),Locus( c(3,3) ),Locus( c(4,3) ),
				Locus( c(2,1) ),Locus( c(2,1) ),Locus( c(1,1) ),Locus( c(1,2) ),Locus( c(1,1) )	)

	strata <- c(rep("Cabo",5), rep("Loreto",5) )
	thePop <- Population( Pop=strata, TPI=loci, PGM=loci2 )

	p1 <- c(2/10,6/10,2/10,0/10)
	p2 <- c(1/10,2/10,6/10,1/10)
	p.tot <-c(3/20,8/20,8/20,1/20)
	
	p1a    <- c(0/10,0/10,6/10,4/10)
	p2a    <- c(7/10,3/10,0/10,0/10)
	p.tota <- c(7/20,3/20,6/20,4/20)

	k <- 2
	
	ht <- 1- sum( p.tot^2 )
	hta <- 1-sum( p.tota^2 )
	
	hs <- ((1-sum(p1^2))+(1-sum(p2^2)))/2
	hsa <- ((1-sum(p1a^2))+(1-sum(p2a^2)))/2
	
	n.harmonic <- 1/mean(1/table(as.factor(strata)))
	
	hs.estimated <- (2*n.harmonic)/(2*n.harmonic-1) * hs
	ht.estimated <- ht + hs.estimated/(2*k*n.harmonic)
	
	hs.estimateda <- (2*n.harmonic)/(2*n.harmonic-1) * hsa
	ht.estimateda <- hta + hs.estimateda/(2*k*n.harmonic)
	
	gst.calc <- 1-hs.estimated/ht.estimated
	gst.calca <- 1-hs.estimateda/ht.estimateda
	
	gst <- genetic.structure(thePop,stratum="Pop",mode="Gst",num.perm=0)
	
	checkTrue( is(gst,"genetic.structure") )
	checkEqualsNumeric( gst$estimate[[1]], gst.calc )
	checkEqualsNumeric( gst$estimate[[2]], gst.calca )
	checkEquals(gst$mode,"Gst")
	checkEquals(gst$loci,c("TPI","PGM"))
	
	
	#gstprime
	gst.prime.calc <- ((1-hs.estimated/ht.estimated) * (k-1+hs.estimated) )
	gst.prime.calc <- gst.prime.calc / ((k-1)*(1-hs.estimated))
	gst.prime.calca <- ((1-hs.estimateda/ht.estimateda) * (k-1+hs.estimateda) )
	gst.prime.calca <- gst.prime.calca / ((k-1)*(1-hs.estimateda))
	
	gst.prime <- genetic.structure(thePop,stratum="Pop",mode="Gst.prime",num.perm=0)
	
	checkTrue( is(gst,"genetic.structure") )
	checkEqualsNumeric( gst.prime$estimate[[1]], gst.prime.calc )
	checkEqualsNumeric( gst.prime$estimate[[2]], gst.prime.calca )
	checkEquals(gst.prime$mode,"Gst.prime")
	checkEquals(gst.prime$loci,c("TPI","PGM"))
	
	
	#Dest
	Dest.calc <- ((ht.estimated-hs.estimated) / (1-hs.estimated))
	Dest.calc <- Dest.calc / (k/(k-1))
	Dest.calca <- ((ht.estimateda-hs.estimateda) / (1-hs.estimateda))
	Dest.calca <- Dest.calca / (k/(k-1))


	dest <- genetic.structure(thePop,stratum="Pop",loci=c("TPI","PGM"),mode="Dest",num.perm=0)
	checkTrue( is(gst,"genetic.structure") )
	checkEqualsNumeric( dest$estimate[[1]], Dest.calc )
	checkEquals(dest$mode,"Dest")
	checkEquals(dest$loci,c("TPI","PGM"))

}


test.araptus.structure <- function() {
	load("../data/araptus_attenuatus.rda")
	baja <- araptus_attenuatus[araptus_attenuatus$Species!="CladeB",]
	pops <- partition(baja,"Pop")
	freq <- allele.frequencies(baja,"LTRS")
	pop.freqs <- lapply( pops, function(x,locus) allele.frequencies(x,locus), locus="LTRS")
	k <- length(pops)
	ht <- he( freq$LTRS )
	hs <- mean( unlist( lapply( pop.freqs, function(x) he( x$LTRS ) ) ) )
	print("araptus")
	print(hs)
	print(ht)
	
	n.harmonic <- 1/mean(1/table(as.character(baja$Pop)))
	hs.estimated <- (2*n.harmonic)/(2*n.harmonic-1) * hs
	ht.estimated <- ht + hs.estimated/(2*k*n.harmonic)
	gst.calc <- 1-hs.estimated/ht.estimated
	gst <- genetic.structure(baja,stratum="Pop",loci="LTRS",mode="Gst")
	
	checkEqualsNumeric(gst$estimate[[1]],gst.calc)
	
	
}








