#' Creates google map with pie charts for allele frequencies.
#'
#' This function creates a google map of the 
#' 
#' @param graph A \code{Population} class (from gstudio package).  This graph must have
#'	\code{V(graph)$name}, \code{V(graph)$latitude}, and \code{V(graph)$longitude} vertex
#'	attributes
#' @param filename The name of the file to use (default=pie_on_map.kml)
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @seealso \code{\link{pies.on.map}}
#' @export
#' 
popgraph.on.map <- function(graph, filename="popgraph_on_map.kml")
{
	if(is.null(graph) || !is(graph,"igraph"))
		stop("The graph must be a population graph (from gstudio)")

	vertex.atts <- list.vertex.attributes(graph)
	if( !("latitude" %in% vertex.atts))
		stop("The graph must have a latitude vertex attribute.")
	if( !("longitude" %in% vertex.atts))
		stop("The graph must have a longitude vertex attribute.")
	if( !("name" %in% vertex.atts))
		stop("The graph must have a name vertex attribute.")

	# make the kml file
	kml.header <- '<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">
<Document>
	<name>Popgraph On Map</name>
	'
	kml.footer <- '</Document></kml>'

	#
	# Make a placemark for each edge
	#
	edges <- get.edges(graph,E(graph))
	K <- dim(edges)[1]
	lats <- V(graph)$latitude
	lons <- V(graph)$longitude	
	edge.styles <- '	<StyleMap id="edge-style">
		<Pair>
			<key>normal</key>
			<styleUrl>#edge.norm</styleUrl>
		</Pair>
		<Pair>
			<key>highlight</key>
			<styleUrl>#edge.highlight</styleUrl>
		</Pair>
	</StyleMap>
	<Style id="edge.norm">
		<LineStyle>
			<width>2</width>
		</LineStyle>
	</Style>
	<Style id="edge.highlight">
		<LineStyle>
			<width>4</width>
		</LineStyle>
	</Style>
'	
	raw.placemark <- '	<Placemark>
		<name>%s</name>
		<styleUrl>#edge-style</styleUrl>
		<LineString>
			<tessellate>1</tessellate>
			<coordinates>
				%f,%f,0,%f,%f,0
			</coordinates>
		</LineString>
	</Placemark>
'
	edge.placemarks <- ""
	for(i in 1:K){
		edge.name <- paste("edge",i,sep="-")
		f.lat <- lats[ edges[i,1]+1 ]
		f.lon <- lons[ edges[i,1]+1 ]
		t.lat <- lats[ edges[i,2]+1 ]
		t.lon <- lons[ edges[i,2]+1 ]
		new.place <- sprintf(raw.placemark,edge.name,f.lon,f.lat,t.lon,t.lat)
		edge.placemarks <- paste(edge.placemarks,new.place,sep="")
	}	
	
	
	#
	# Put on the populations
	#
	vertex.styles <- '	<Style id="vertex-norm">
	<IconStyle>
		<scale>0.7</scale>
		<Icon>
			<href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>
		</Icon>
	</IconStyle>
	<ListStyle>
	</ListStyle>
	</Style>
	<Style id="vertex-highlight">
	<IconStyle>
		<scale>1</scale>
		<Icon>
			<href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>
		</Icon>
	</IconStyle>
	<ListStyle>
	</ListStyle>
	</Style>
	<StyleMap id="vertex.map">
		<Pair>
			<key>normal</key>
			<styleUrl>vertex-norm</styleUrl>
		</Pair>
		<Pair>
			<key>highlight</key>
			<styleUrl>vertex-highlight</styleUrl>
		</Pair>
	</StyleMap>
'
	vertex.placemark <- '	<Placemark>
		<name>%s</name>
		<styleUrl>#vertex.map</styleUrl>
		<Point>
			<coordinates>%f,%f,0</coordinates>
		</Point>
	</Placemark>
'
	pops <- V(graph)$name
	vertex.placemarks <- ''
	for(i in 1:length(pops)){
		pt <- sprintf(vertex.placemark,pops[i],lons[i],lats[i])
		vertex.placemarks <- paste( vertex.placemarks, pt, sep="")
	}
	
	kml <- paste(kml.header,edge.styles,sep="")
	kml <- paste(kml,vertex.styles,sep="")
	
	kml <- paste(kml,edge.placemarks,sep="")
	kml <- paste(kml,vertex.placemarks,sep="")	
	kml <- paste(kml,kml.footer,sep="")
	
	
	cat(kml,file=filename)
}


