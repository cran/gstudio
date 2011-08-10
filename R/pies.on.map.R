#' Creates google map with pie charts for allele frequencies.
#'
#' This function creates a google map of the 
#' 
#' @param filename The name of the file to use (default=pie_on_map.kml)
#' @param pop A \code{Population} class (from gstudio package)
#' @param stratum The stratum to plot
#' @param loci The locus to use
#' @param lat The label for the latitude data column in pop if not "lat"
#' @param lon The label for the longitude data column in po if not "lon"
#' @export
#' 
pies.on.map <- function(filename="pies_on_map.kml",
						pop=NULL,
						stratum=NULL,
						loci=NULL,
						lat="Latitude",
						lon="Longitude") 
{
	if(is.null(pop) || !is(pop,"Population"))
		stop("Use a Population class (from gstudio) for this function")
	if( is.null(stratum) || !(stratum %in% names(pop) ))
		stop("You must supply a stratum that is actually in the population")
	if( is.null(loci) )
		stop("You need to specify a Locus class (from gstudio) for this function")
	require(gstudio)
	allfreqs <- allele.frequencies( pop, loci )
	pops <- partition(pop,stratum)


	# make the kml file
	
	kml.header <- '<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">
<Document>
	<name>Pies On Map</name>'
	kml.footer <- '</Document></kml>'
	styles <- ""
	placemarks <- list()
	
	for( popname in names(pops) ){
		subpop <- pop[pop[[stratum]]==popname,]
		subpop.freq <- allele.frequencies(subpop,loci)
		subpop.lat <- mean(subpop[[lat]])
		subpop.lon <- mean(subpop[[lon]])
		for( locusName in names(subpop.freq) ){
			if(!(locusName %in% names(placemarks)) ) {
				placemarks[[locusName]] <- ""
			}
			
			alleles <- sort( names( subpop.freq[[locusName]] ) )
			
			f <- get.frequencies( subpop.freq[[locusName]],alleles)
			
			icon.url1 <- google.pie.chart(x=f)
			icon.url2 <- google.pie.chart(x=f,labels=alleles)
			subpop.locus.name <- paste(popname,locusName,sep=".")
			placemark <- .get.kml.placemark(subpop.locus.name,subpop.lat,subpop.lon)
			style <- .get.kml.style(subpop.locus.name,icon.url1,icon.url2)
			
			styles <- paste(styles,style,sep="\n")
			placemarks[[locusName]] <- paste(placemarks[[locusName]],placemark,sep="\n")	
		}
	}
	
	
	kml <- paste(kml.header,styles,"\n",sep="")
	
	for( lname in loci ){
		folder <- sprintf("<Folder>\n<name>%s</name>%s</Folder>\n",lname,placemarks[[lname]])
		kml <- paste(kml,folder,sep="")
	}
		
	kml <- paste(kml,kml.footer,sep="\n")
	
	cat(kml,file=filename)
}

#' Helper function for creating styles
#'
#' @param name The style name
#' @param url1 The url for the normal icon
#' @param url2 The url for the highlight icon
#' @return A full style-norm, style-highlight, style-map text group
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#
.get.kml.style <- function(name,url1,url2) {
	normName <- paste(name,"norm",sep="-")
	highlightName <- paste(name,"highlight",sep="-")
	baseStyleMap <- '<StyleMap id="%s.map">
	<Pair>
		<key>normal</key>
		<styleUrl>%s</styleUrl>
	</Pair>
	<Pair>
		<key>highlight</key>
		<styleUrl>%s</styleUrl>
	</Pair>
</StyleMap>'
	
	baseStyle <- '<Style id="%s">
	<IconStyle>
		<scale>%.1f</scale>
		<Icon>
			<href>%s</href>
		</Icon>
	</IconStyle>
	<ListStyle>
	</ListStyle>
</Style>'
	
	ret.norm <- sprintf(baseStyle,normName,1.1,url1)
	ret.high <- sprintf(baseStyle,highlightName,1.1,url2)
	ret.map <- sprintf(baseStyleMap,name,normName,highlightName)
	
	ret <- paste(ret.norm,ret.high,ret.map,sep="\n")
	return(ret)
}

#' Helper function for creating placemarks
#'
#' @param name The placemark name
#' @param lat The latitude
#' @param lon The longitude
#' @return A full placemark XML block
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#
.get.kml.placemark <- function( name, lat, lon ){
	base.placemark='	<Placemark>
		<name>%s</name>
		<styleUrl>#%s.map</styleUrl>
		<Point>
			<coordinates>%f,%f,0</coordinates>
		</Point>
	</Placemark>'
	ret <- sprintf(base.placemark,name,name,lon,lat)
	return(ret)
}


