##############################################################################
# 							    Read PGraph                              
#                                                                             
# 						    R.J. Dyer <rjdyer@@vcu.edu>                        
#                                                                             
##############################################################################

#' Loads a population graph file *.pgraph
#'
#' General function to load a population graph file and turn it into a population
#'	graph object.
#'
#' @param file The full path to the pgraph file
#' @return A fully formed \code{igraph} object representing the population
#'	graph
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
read.pgraph <- function( file ) {
	raw <- read.table(file=file,header=TRUE,sep=":",stringsAsFactors=FALSE)

	# parse header
	meta <- strtoi(unlist(strsplit(unlist(strsplit(names(raw),"X"))[2],"\\.")))
	
	# make nodes
	nodes <- character(meta[1])
	node_size <- numeric(meta[1])
	for(i in 1:meta[1]) {
		rawRow <- raw[i,]
		row <- unlist(strsplit(rawRow,"[[:space:]]"))
		nodes[i] <- row[1]
		node_size[i] <- as.numeric(row[2])
	}

	A <- matrix(0,nrow=meta[1],ncol=meta[1])
	for( i in (meta[1]+1):(meta[1]+meta[2])){
		rawRow <- raw[i,]
		row <- unlist(strsplit(rawRow,"[[:space:]]"))
		from_node <- which(nodes==row[1])
		to_node <- which(nodes==row[2])
		wt <- as.numeric(row[3])
		A[from_node,to_node] <- A[to_node,from_node] <- wt
	}
	
	graph <- graph.adjacency(A,mode="undirected",weighted=TRUE)
	V(graph)$size <- node_size
	V(graph)$name <- nodes
	
	return(graph)
}
