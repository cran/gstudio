test.google_pie_chart <- function() {
	link.true <- 'http://chart.apis.google.com/chart?chf=bg,s,00000000&amp;chs=75x75&amp;cht=p&amp;chd=t:17,33,50&amp;chco=4362FB'	
	link <- google.pie.chart(c(1,2,3))
	checkEquals(link,link.true)
}

