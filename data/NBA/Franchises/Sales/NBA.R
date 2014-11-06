library(data.table)
library(rCharts)
library(reshape2)
for(i in 10:15){
	print(70*i)
}

for(i in .05:.07){
	print(70/i)
}

files<-list.files()
sales<-read.csv(files[2])
salesplot<-sales


salesplot$Price<-as.numeric(gsub(",","",salesplot$Price))/1000000
salesplot$Inflation_Adusted_Price<-as.numeric(gsub(",","",salesplot$Inflation_Adusted_Price))/1000000
salesplot<-salesplot[salesplot$Price>0,]
salesplot<-salesplot[order(salesplot$Year),]
names(salesplot)
sp<-salesplot[,c('Year','Name','Price','Buyer','Details','Current_FranchiseURL','Pic_URL')]
splist<-as.list(sp)

nba<-data.table(
	x=sp$Year,
	y=sp$Price,
	name = sprintf("<table cellpadding='4' style='line-height:1.5'><tr><th colspan='3'>%1$s</th></tr><tr><td><img src='%2$s' height='100' width='75'></td><td align='left'>Year: %4$s<br>Team: %1$s<br>Buyer: %5$s<br>Price: %3$s</td></tr></table>", 
								 sp$Name,
								 sp$Pic_URL,
								 sp$Price,
								 sp$Year,
								 sp$Buyer,
								 sp$Details
								 ),
	url = sp$Current_FranchiseURL,
	team = sp$Name
)
	)
nba

teamSeries <- lapply(split(nba, nba$team), function(x) {
	res <- lapply(split(x, rownames(x)), as.list)
	names(res) <- NULL
	return(res)
})
teamSeries

b <- rCharts::Highcharts$new()
invisible(sapply(teamSeries, function(x) {
	b$series(data = x, type = ("scatter"), name = x[[1]]$team)
}
))
b

b$plotOptions(
	scatter = list(
		cursor = "pointer", 
		point = list(
			events = list(
				click = "#! function() { window.open(this.options.url); } !#")), 
		marker = list(
			symbol = "circle", 
			radius = 5
		)
	)
)
b
b$xAxis(title = list(text = "Year"), labels = list(format = "{value}"))
b$yAxis(title = list(text = "Purchase Price in Millions"), labels = list(format = "{value} $"))
b
b$tooltip(useHTML = T, formatter = "#! function() { return this.point.name; } !#")
b
b$title(text = "History of NBA Franchise Sales")
b$subtitle(text = "Aragorn Technologies and Alex Bresler")
b
b$params$width<-800
b$params$height<-500

	b
b$publish('History of NBA Transactions',host='gist')