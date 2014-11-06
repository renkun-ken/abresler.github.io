list.files()
codes <- read.csv(list.files()[2])
codes <- codes[,1:2]
franchises <- read.csv(list.files()[5])
franchises <- subset(franchises, active == T)
cols <- names(franchises)[c(5,7,11)]
franchises <- franchises[,cols]
team_colors_pics <- merge(codes,franchises)
teams_ids <-unique(team_colors_pics$team_id)
names(team_colors_pics)[3] <- 'team'

t <- data.frame(cap_dt[ , list(salary_millions = sum(salary)/1000000), by = list( team)])
salaries <- data.frame(cap_dt[ , list(salary_millions = sum(salary)/1000000), by = list(season, team)])
totals_salaries <- (aggregate(salary_millions~team,data=cap_salaries,sum))
names(totals_salaries)[2] <- 'total_salary_millions'

totals <- merge(t,team_colors_pics)
totals <- totals[order(totals$salary_millions,decreasing=T),]
totals$label <- totals$team
totals$label <- gsub("Timberwolves","Wolves",totals$label)
totals$label <- gsub("Trail Blazers","Blazers",totals$label)
totals$label <- gsub("Mavericks","Mavs",totals$label)
totals$label <- gsub("Sixers","76ers",totals$label)
totals$label <- gsub("Cavaliers","Cavs",totals$label)

totals$detail <- 
	sprintf("<table cellpadding='2' style='line-height:1.25'><tr><th colspan='2'>%1$s</th></tr><tr><td><img src='%2$s' height='125' width='125'></td><td align='center'>Salary Commitment (2013-2019): $%3$s</td></tr></table>",
	totals$label,
	totals$logo_image_url,
	round(totals$salary_millions,digits=2)
)
totals$detail
colors <-totals$primary
p <- nPlot(salary_millions~label, group = 'label', data = totals, type = 'multiBarHorizontalChart')
p$params$width <- 750
p$params$height <- 500
p$chart(stacked = TRUE, showControls=F)
p$chart(color=colors)
p$chart(tooltip = "#! function(key, x, y, e, graph) {
  return '' + e.point.detail + ''
}!#")
p
p$publish('All NBA Salaries', host = 'gist')
