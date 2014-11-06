packages = c('dplyr','pipeR','reshape2','data.table')
lapply(packages,library,character.only = T)
s = "2013-2014"
min_games = 10
min_minutes = 400
setwd("~/Desktop/Github/asb_shiny_apps/nba/explorer/v3")
'data/per_game_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
	filter(season == s) %>>%
	filter(g >= min_games) %>>% 
	select(-table_name,-data_source,-scrape_time) -> pg
pg$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> pg$pos)

'data/advanced_stats_1951_2014.csv' %>>% fread(stringsAsFactors = F) %>>% tbl_df() %>>%
	filter(season == s) %>>%
	filter(g >= min_games) %>>% 
	filter(mp >= min_minutes) %>>%
	select(-table_name,-data_source,-scrape_time,-mp) ->adv
adv %>>% filter(bref_team_id == "BRK")
pg %>>% filter(bref_team_id == "BRK")
adv$pos %>>% (colsplit(string = .,pattern = '\\-',names = c('pos_1','pos_2'))) %>>% (.$pos_1 -> adv$pos)
merge(pg,adv, by = names(pg)[names(pg) %in% names(adv)]) -> all
all$pos  %>>% factor(levels = c("PG","SG","SF","PF","C")) -> all$pos

all[is.na(all)] <- 0

all$x2p. <- NULL
all$x3p. <- NULL
apply(
	all[,!names(all) %in% c('player','season','bref_team_id','pos','age')],2,function(x) as.numeric(x)
	) -> all2
all2 %>>% scale -> all_k
ssPlot <- function(data, maxCluster = 9) {
	# Initialize within sum of squares
	SSw <- (nrow(data) - 1) * sum(apply(data, 2, var))
	SSw <- vector()
	for (i in 2:maxCluster) {
		SSw[i] <- sum(kmeans(data, centers = i)$withinss)
	}
	plot(1:maxCluster, SSw, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
}
all_k %>>% ssPlot()


all_k %>>% kmeans(5) -> fit

# append cluster assignment
all %>>% (data.frame(.,fit$cluster)) -> all_clustered
all_clustered$fit.cluster %>>%  as.factor() -> all_clustered$fit.cluster

all_clustered %>>%
	filter(bref_team_id == 'BRK') %>>%
	select(player,pos,fit.cluster) %>>%
	arrange(desc(fit.cluster))


#Cluster centers can inform on how taste profiles differ between clusters.
fit$betweenss

#Based on these centers, I anticipate that my love for the full bodied, smoky and medicinal lies in cluster 4.
update.packages()
