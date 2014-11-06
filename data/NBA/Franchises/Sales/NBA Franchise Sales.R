library(rCharts)
library(data.table)
library(plyr)

sales <- data.frame(read.csv(list.files()[3]),stringsAsFactors=F)
sales$price <- sales$price/1000000
sales$inflation_adusted_price <-sales$inflation_adusted_price/1000000
sales.plot <- sales[sales$price>0,]
sales.plot <- sales.plot[order(sales.plot$year,decreasing=F) , ]
names(sales.plot)
sales.plot <- sales.plot[, c(2:3,6,10,8:9,14)]
sp.list <- as.list(sales.plot)
names(sales)
teams.colors <- unique(sales[c('name','colors')])
teams.colors <- teams.colors[order(teams.colors$name,decreasing=F),]

ds <- data.table(
	x=sales.plot$year,
	y=sales.plot$price,
	name = sprintf("<table cellpadding='4' style='line-height:1.5'><tr><th colspan='4'>%1$s</th></tr><tr><td><img src='%2$s' height='125' width='100'></td><td align='left'>Year: %4$s<br>Team: %1$s<br>Buyer(s): %5$s<br>Price in Millions:$%3$s<br>Details: %6$s</td></tr></table>", 
								 sales.plot$franchise,
								 sales.plot$current_pic_url,
								 sales.plot$price,
								 sales.plot$year,
								 sales.plot$buyer,
								 sales.plot$details
								 ),
	team = sales.plot$name
)
teamSeries <- lapply(split(ds, ds$team), function(x) {
	res <- lapply(split(x, rownames(x)), as.list)
	names(res) <- NULL
	return(res)
})

b <- rCharts::Highcharts$new()
invisible(sapply(teamSeries, function(x) {
	b$series(data = x, type = ("scatter"), name = x[[1]]$team)
}
))

b$plotOptions(
	scatter = list(
		cursor = "pointer", 
		marker = list(
			symbol = "circle", 
			radius = 6.5
		)
	)
)

b$xAxis(title = list(text = "Year",style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif', color = 'black')), labels = list(format = "{value}"))
b$yAxis(title = list(text = "Purchase Price in Millions",style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif', color = 'crimson')), labels = list(format = "${value}"))

b$tooltip(useHTML = T, formatter = "#! function() { return this.point.name; } !#")
b$legend(align = 'right', verticalAlign = 'middle', layout = 'vertical')
b$params$height <- 500
b$params$width <-850
b$colors('rgba(255,127,80,1)','rgba(0,100,0,1)','rgba(255,0,0,1)','rgba(218,165,32,1)','rgba(34,139,34,1)','rgba(0,191,255,1)','rgba(218,165,32,1)','rgba(220,20,60,1)','rgba(238,32,77,1)','rgba(0,0,128,1)','rgba(148,0,211,1)','rgba(255,140,0,1)','rgba(120,91,169,1)','rgba(127,154,234,1)','rgba(49,92,221,1)','rgba(1,1,1,1)','rgba(192,207,235,1)','rgba(231,189,66,1)','rgba(188,157,88,1)','rgba(21,76,135,1)','rgba(120,91,169,1)','rgba(196,52,45,1)','rgba(41,57,123,1)','rgba(144,144,144,1)','rgba(204,85,0,1)','rgba(17,121,171,1)','rgba(35,0,143,1)','rgba(128,0,0,1)','rgba(255,218,104,1)','rgba(0,0,128,1)')
b
write.csv(sales.plot,'All Non Internal Transfer NBA Sales.csv'
b$publish('History of NBA Transactions',host='gist')	
