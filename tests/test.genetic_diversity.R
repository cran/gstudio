test.genetic.diversity <- function() {
	baja <- araptus_attenuatus[araptus_attenuatus$Species=="CladeC",]
	gdiv <- genetic.diversity(baja,stratum="Cluster",loci="MP20",mode="A95")
	
	checkEquals(gdiv$mode,"A95")
	checkEquals(gdiv$stratum,"Cluster")
	checkEquals(gdiv$loci,"MP20")
	checkTrue(all( as.character(gdiv$estimate) == c(4,2,2) ) )
}