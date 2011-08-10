test.popgraph <- function() {
	
	baja <- araptus_attenuatus[araptus_attenuatus$Species!="CladeB",]
	
	graph <- population.graph(baja,"Pop")
	cl <- clusters(graph)
	
	checkEqualsNumeric( cl$no, 3 )
	checkEqualsNumeric( max(cl$csize), 26 )
	checkTrue( all(table(cl$membership)==c(26,9,1)))
	
	checkEqualsNumeric( length(E(graph)), 59 )
	checkEqualsNumeric( length(V(graph)), 36 )
	checkTrue( all(as.character(sort(V(graph)$name)) == sort(as.character(levels(baja$Pop)))))
	
}