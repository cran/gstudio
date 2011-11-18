test.congruence.probability <- function() {
	
	A <- matrix(0,nrow=4,ncol=4)
	A[1,2] <- A[2,1] <- A[1,3] <- A[3,1] <- A[4,1] <- A[1,4] <- A[2,4] <- A[4,2] <- 1
	B <- matrix(0,nrow=4,ncol=4)
	B[1,3] <- B[3,1] <- B[1,4] <- B[4,1] <- B[4,2] <- B[2,4] <- B[3,4] <- B[3,2] <- 1	
	C <- A * B
	
	row.names(A) <- row.names(B) <- row.names(C) <- colnames(A) <- colnames(B) <- colnames(C) <- c("A","B","C","D")
	
	
	mA <- sum(A)/2
	mB <- sum(B)/2
	mC <- sum(C)/2
	N <- 4
	mMax <- N*(N-1)/2
	ell <- mA+mB-mMax
	i <- max(0,ell):mA
	
	p.top <- choose( mA,mC ) * choose( mMax-mA, mB-mC )
	p.bot <- sum( choose(mA,i) * choose( mMax-mA, mB-i ))
	
	p.calc <- sum(p.top/p.bot)
	
	print("calc")
	print(p.calc)
	
	graphA <- graph.adjacency(A,mode="undirected")
	graphB <- graph.adjacency(B,mode="undirected")
	graphC <- graph.adjacency(C,mode="undirected")
	
	p <- congruence.probability(graphA,graphB,graphC)
	print("obs")
	print(p)
	checkEqualsNumeric( p, p )
}
