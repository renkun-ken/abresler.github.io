options(viewer = NULL)
library(rCharts)
all = tbl_dt(read.csv('final_nba_sales_history.csv'))
g0 = tbl_dt(all[price>0,,])
names(g0)
g = 
	g0 %>%
	mutate(
		price_log10 = log10(price),
		price_2014_dollars_log10 = log10(price_2014_dollars)
		)

g = tbl_df(g)
g[,15:17] = apply(g[,15:17],2, function(x) x/1000)

names(g)
names(g)[15:17] = 
	paste(names(g[15:17]),'thousands', sep = '_')
names(g)[15:17]
g$size = (scale(g$price_2014_dollars_thousands,center = 8)/1.5)+1
g = data.table(g)
setkeyv(g,'name')
tc = unique(g[,list(name,color)])
g$date = 
	paste(g$year_sold,g$month_sold, sep = '.')
setkeyv(g,'date')
setkeyv(g,'name')


g$detail <-
	sprintf("<html><head><title></title></head><body><table border='2' cellpadding='4' style='line-height:1.55'><tbody><tr><th colspan='5.5'><span style='font-family:times new roman,times,serif;'><span style='font-size:18px;'>%1$s</span></span></th></tr><tr><td style='text-align: center;'><span style='font-family:times new roman,times,serif;'><img src=%2$s width='225'/></span></td><td style='text-align: center;'><span style='font-family:times new roman,times,serif;'><span style='font-size:10px;'><strong>Current Team:</strong> %3$s<br /><strong>Sale Month.Year:</strong> %4$s<br /><strong>Sale Price (thousands):</strong> $%5$s<br /><strong>Sale Price 2014 (thousands):</strong> $%6$s<br /><strong>Profit/Loss(Thousands):</strong> $%7$s<br /><strong>Buyer(s):</strong> %8$s<br /><strong>Purchase Details:</strong> %9$s<br /><strong>Expansion:</strong> %10$s<br /><strong>Arena Included:</strong> %11$s</span></span></td></tr></tbody></table></body></html>",
					g$franchise,
					g$current_pic_url,
					g$team,
					g$date,
					Trim(prettyNum(g$price_thousands, big.mark = '\\,')),
					Trim(prettyNum(g$price_2014_dollars_thousands, big.mark = '\\,')),
					Trim(prettyNum(g$price_change_thousands, big.mark = '\\,')),
					g$buyer,
					g$details,
					g$expansion,
					g$arena_included
					)



setkeyv(g,'name')
g$detail[102]
n1 <- 
	nPlot(price_thousands ~ date, data = g, group = "name", type = "scatterChart")
n1
n1$chart(showControls = F) #Turns off the controller that shows grouped/stack
n1$chart(showDistX = T) #Freezes x-axis & shows tick
n1$chart(showDistY = T)
n1$chart(size = '#! function(d){return n1d.size} !#')
n1$yAxis( tickFormat="#!function(d) {return d3.format(',.02f')(d)}!#" )
n1
n1$chart(color = tc$color)
n1$addControls("y", value = "price_thousands", 
							 values = names(g)[c(15,29,16,30,17)]
)
n1$addControls("group", value = "name", 
							 values = names(g)[c(8,12,13,21,22)])
n1
n1$addFilters('arena_included','expansion','decade_sold','conference','division','franchise','team')
n1$templates$script = "http://timelyportfolio.github.io/rCharts_nvd3_templates/script_multiselect.html"
n1$chart(tooltipContent = "#! function(key, x, y, e){
	return '<p>' + e.point.detail + '</p>' 
	'<p><b>'+ 'yAxis: '+ '</b>' + y +'</p>'
				 } !#")
n1$publish('sales', host = 'gist')
