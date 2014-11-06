## from point of neighbors df
require(data.table);require(rCharts)
dt = data.table(userNeighbors.df)
dt[location == '',location := 'Unknown']

dt$detail =
	sprintf("<table border='2' cellpadding='4' style='line-height:1.55'><tbody><tr><th colspan='5.5'><span style='font-size:12px;'><span style='font-family:lucida sans unicode,lucida grande,sans-serif;'>%1$s</span></span></th></tr><tr><td><span style='font-size:8px;'><span style='font-family:lucida sans unicode,lucida grande,sans-serif;'><img src='%2$s' height='60' width='60'></td><td style='text-align: justify;'><span style='font-size:8px;'><span style='font-family:lucida sans unicode,lucida grande,sans-serif;'><strong>Screen Name:</strong> %3$s<br><strong>Location:</strong> %4$s<br><strong>Follower(s):</strong> %5$s<br><strong>Friend(s):</strong> %6$s<br><strong>Joined Twitter:</strong> $%8$s<br><strong>Description:</strong> $%7$s</span></td></tr></tbody></table>",
					dt$name,
					dt$profileImageUrl,
					dt$screenName,
					dt$location,
					dt$followersCount,
					dt$friendsCount,
					dt$description,
					dt$created
	)
dt =
	dt[,c(2:5,9:10,13:15,17:20), with = F]
setkey(dt,'cluster')
colors = c('#000000','#ff0000','#b8860b','#0000ff','#008000','#800080')
p2 <- nPlot(logFollowersCount ~ logFriendsCount, group = 'cluster', data = dt, type = 'scatterChart')
p2$yAxis(axisLabel = '')
p2$xAxis(axisLabel = '')
p2$chart(color = colors)
p2$chart(showControls = F) #Turns off the controller that shows grouped/stack
p2$chart(showDistX = T) #Freezes x-axis & shows tick
p2$chart(showDistY = T)
p2$yAxis( tickFormat="#!function(d) {return d3.format(',f')(d)}!#" )
p2$xAxis( tickFormat="#!function(d) {return d3.format(',f')(d)}!#" )
p2$chart(tooltipContent = "#! function(key, x, y, e){
	return '<p>' + e.point.detail + '</p>'
	'<p><b>'+ 'yAxis: '+ '</b>' + y +'</p>'
				 } !#")
p2$addControls("x", value = "Log Friends",
							 values = names(dt)[c(11,4,12,7,10,2,3,8,1,9)])
p2$addControls("y", value = "Log Friends",
							 values = names(dt)[c(10,2,3,1,8,7,12,11,4)])
p2$addControls("group", value = "cluster",
							 values = names(dt)[c(12,6,5)])
p2$addFilters('cluster')
p2$templates$script = "http://timelyportfolio.github.io/rCharts_nvd3_templates/script_multiselect.html"
